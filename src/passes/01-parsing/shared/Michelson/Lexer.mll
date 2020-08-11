(* Lexer specification for Michelson, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos

(* TOKENS *)

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. Consequently, generic functions to
   construct tokens are provided. Note predicate [is_eof], which
   caracterises the virtual token for end-of-file, because it requires
   special handling. *)

type lexeme = string

module type TOKEN =
  sig
    type token

    (* Errors *)

    type int_err = Non_canonical_zero

    type annot_err = Annotation_length of int

    type ident_err =
      Valid_prefix          of Pair.index * Pair.tree
    | Invalid_tree          of Pair.index * char * Pair.tree
    | Truncated_encoding    of Pair.index * Pair.child * Pair.tree
    | Invalid_Roman_numeral of int
    | Missing_break         of int
    | Invalid_identifier

    (* Injections *)

    val mk_string : lexeme -> Region.t -> token
    val mk_bytes  : lexeme -> Region.t -> token
    val mk_int    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident  : lexeme -> Region.t -> (token, ident_err) result
    val mk_annot  : lexeme -> Region.t -> (token, annot_err) result
    val mk_sym    : lexeme -> Region.t -> token
    val eof       : Region.t -> token

    (* Predicates *)

    val is_eof    : token -> bool

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
    val to_region : token -> Region.t

    (* Style *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg

    val check_right_context :
      token ->
      (Lexing.lexbuf -> (Markup.t list * token) option) ->
      Lexing.lexbuf ->
      unit
  end

(* The functorised interface

   Note that the module parameter [Token] is re-exported as a
   submodule in [S].
 *)

module type S =
  sig
    module Token : TOKEN
    type token = Token.token

    val scan :
      token LexerLib.state -> Lexing.lexbuf -> token LexerLib.state

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool -> [`Byte | `Point] ->
      error Region.reg -> file:bool -> string Region.reg
  end

module Make (Token : TOKEN) : (S with module Token = Token) =
  struct
    module Token = Token
    type token = Token.token

    (* ERRORS *)

    type error =
      Invalid_utf8_sequence
    | Unexpected_character of char
    | Undefined_escape_sequence
    | Missing_break
    | Unterminated_string
    | Unterminated_integer
    | Odd_lengthed_bytes
    | Unterminated_comment
    | Annotation_length of int
    | Invalid_identifier
    | Orphan_minus
    | Non_canonical_zero
    | Negative_byte_sequence
    | Broken_string
    | Invalid_character_in_string
    | Invalid_Roman_numeral
    | Valid_prefix of Pair.tree
    | Invalid_tree of char * Pair.tree
    | Truncated_encoding of Pair.child * Pair.tree

    let sprintf = Printf.sprintf

    let error_to_string = function
      Invalid_utf8_sequence ->
        "Invalid UTF-8 sequence.\n"
    | Unexpected_character c ->
        sprintf "Unexpected character '%c'.\n" c
    | Undefined_escape_sequence ->
        "Undefined escape sequence.\n\
         Hint: Remove or replace the sequence.\n"
    | Missing_break ->
        "Missing break.\n\
         Hint: Insert some space.\n"
    | Unterminated_string ->
        "Unterminated string.\n\
         Hint: Close with double quotes.\n"
    | Unterminated_integer ->
        "Unterminated integer.\n\
         Hint: Remove the sign or proceed with a natural number.\n"
    | Odd_lengthed_bytes ->
        "The length of the byte sequence is an odd number.\n\
         Hint: Add or remove a digit.\n"
    | Unterminated_comment ->
        "Unterminated comment.\n\
         Hint: Close with \"*/\".\n"
    | Annotation_length max ->
        sprintf "Annotation length exceeds the built-in limit %d.\n" max
    | Orphan_minus ->
        "Orphan minus sign.\n\
         Hint: Remove the trailing space.\n"
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0.\n"
    | Negative_byte_sequence ->
        "Negative byte sequence.\n\
         Hint: Remove the leading minus sign.\n"
    | Broken_string ->
        "The string starting here is interrupted by a line break.\n\
         Hint: Remove the break or close the string before.\n"
    | Invalid_character_in_string ->
        "Invalid character in string.\n\
         Hint: Remove or replace the character.\n"
    | Invalid_Roman_numeral ->
        "Invalid or out-of-range Roman numeral (from 1 to 999).\n\
         (Numerus pravus/nimius.)\n"
    | Invalid_identifier ->    (* TODO: Shortest edit distance? *)
        "Invalid identifier.\n"
    | Invalid_tree (char, tree) ->
        let opposite = if char = 'I' then 'A' else 'I' in
        sprintf "\
          Wrong pair constructor '%c'.\n\
          Hint: Try inserting to its left '%c', or remove it.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          char opposite "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree completed with Xs:\n%s" Pair.(to_string tree))

    | Truncated_encoding (child, tree) ->
        let expected =
          match child with
            `Left -> 'A'
          | `Right -> 'I' in
        sprintf "\
          Unexpected end of encoding.\n\
          Hint: Try inserting to its left '%c' or 'P'.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          expected "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree completed with Xs:\n%s" Pair.(to_string tree))

    | Valid_prefix tree ->
        sprintf "\
          Extraneous pair constructor.\n\
          Hint: Try removing it.\n\
          Anyhow, follow the following BNF grammar:\n\
          <pair macro> ::= P <left> <right> R\n\
          %s    <left> ::= A | P <left> <right>\n\
          %s   <right> ::= I | P <left> <right>.%s"
          "  " "  "
          (sprintf "\nLean your head to the left and consider \
                    the tree so far:\n%s" Pair.(to_string tree))

    | _ -> assert false

    exception Error of error Region.reg

    let format_error ?(offsets=true) mode Region.{region; value} ~file =
      let msg = error_to_string value
      and reg = region#to_string ~file ~offsets mode in
      let value = sprintf "Lexical error %s:\n%s\n" reg msg
      in Region.{value; region}

    let fail region value = raise (Error Region.{region; value})

    (* TOKENS *)

    (* Making tokens *)

    let mk_string (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_string lexeme region
      in state#enqueue token

    let mk_bytes bytes state buffer =
      let region, _, state = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in state#enqueue token

    let mk_int state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let region, lexeme, state = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural

    let mk_ident state buffer =
      let mk_region index start =
        let start = start#shift_bytes index in
        let stop  = start#shift_bytes 1
        in Region.make ~start ~stop
      and start = state.pos in
      let region, lexeme, state = state#sync state buffer in
      match Token.mk_ident lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Valid_prefix (index, tree) ->
          let region = mk_region index start in
          raise (Error Region.{region; value = Valid_prefix tree})
      | Error Token.Invalid_tree (index, char, tree) ->
          let region = mk_region index start
          and value  = Invalid_tree (char, tree)
          in raise (Error Region.{region; value})
      | Error Token.Truncated_encoding (index, child, tree) ->
          let region = mk_region index start
          and value  = Truncated_encoding (child, tree)
          in raise (Error Region.{region; value})
      | Error Token.Invalid_identifier ->
          fail region Invalid_identifier
      | Error Token.Invalid_Roman_numeral index ->
          let region = mk_region index start
          in fail region Invalid_Roman_numeral
      | Error Token.Missing_break index ->
          let start  = start#shift_bytes index in
          let region = Region.make ~start ~stop:start
          in fail region Missing_break

    let mk_annot state buffer =
      let region, lexeme, state = state#sync state buffer
      in match Token.mk_annot lexeme region with
           Ok token -> state#enqueue token
         | Error Token.Annotation_length max ->
             fail region (Annotation_length max)

    let mk_sym state buffer =
      let region, lexeme, state = state#sync state buffer
      in Token.mk_sym lexeme region, state

    let mk_eof state buffer =
      let region, _, state = state#sync buffer in
      let token = Token.eof region
      in state#enqueue token

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions

   For annotations, see documentation at
   http://tezos.gitlab.io/master/whitedoc/michelson.html#syntax
*)

let utf8_bom   = "\xEF\xBB\xBF" (* Byte Order Mark for UTF-8 *)
let nl         = ['\n' '\r'] | "\r\n"
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let integer    = '-'? natural
let letter     = ['a'-'z' 'A'-'Z']
let ident      = small (letter | '_' | digit)*
let hexa_digit = digit | ['A'-'F']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as seq)
let esc        = "\\n" | "\\\"" | "\\\\" | "\\b"
               | "\\r" | "\\t" | "\\x" byte
let symbol     = ';' | '(' | ')' | '{' | '}'
let annotation =
  (':'|'@'|'%')
  ('@'|'%'|"%%"| ('_' | letter) ('_' | letter | digit | '.')*)?

(* RULES *)

(* Except for the first rule [init], all rules bear a name starting
   with "scan".

   All have a parameter [state] that they thread through their
   recursive calls. The rules for the structured constructs (strings
   and comments) have an extra parameter of type [thread] (see above).
*)

rule init state = parse
  utf8_bom   { scan (state#push_bom lexbuf) lexbuf         }
| _          { LexerLib.rollback lexbuf; scan state lexbuf }

and scan state = parse
  nl         { scan (state#push_newline lexbuf) lexbuf }
| ' '+       { scan (state#push_space   lexbuf) lexbuf }
| '\t'+      { scan (state#push_tabs    lexbuf) lexbuf }

| ident      { mk_ident     state lexbuf }
| bytes      { mk_bytes seq state lexbuf }
| integer    { mk_int       state lexbuf }
| annotation { mk_annot     state lexbuf }
| symbol     { mk_sym       state lexbuf }
| eof        { mk_eof       state lexbuf }

(* Strings *)

| '"'  { let opening, _, state = state#sync lexbuf in
         let thread = LexerLib.mk_thread opening in
         scan_string thread state lexbuf |> mk_string }

(* Block comments *)

| "/*" { let opening, _, state = state#sync lexbuf in
         let thread            = LexerLib.mk_thread opening in
         let thread            = thread#push_string lexeme in
         let thread, state     = scan_block thread state lexbuf
         in scan (state#push_block thread) lexbuf }

(* Line comments *)

| '#'  { let opening, _, state = state#sync lexbuf in
         let thread            = LexerLib.mk_thread opening in
         let thread            = thread#push_string lexeme in
         let thread, state     = scan_line thread state lexbuf
         in scan (state#push_line thread) lexbuf }

(*
  (* Some special errors

     Some special errors are recognised in the semantic actions of the
     following regular expressions. The first error is a minus sign
     separated from the integer it modifies by some markup (space or
     tabs). The second is a minus sign immediately followed by
     anything else than a natural number (matched above) or markup and
     a number (previous error). The third is the strange occurrence of
     an attempt at defining a negative byte sequence. Finally, the
     catch-all rule reports unexpected characters in the buffer (and
     is not so special, after all).
  *)

| '-' { let region, _, state = state#sync lexbuf in
        let state = scan state lexbuf in
        let open Markup in
        match FQueue.peek state.units with
          None -> assert false
        | Some (_, ((Space _ | Tabs _)::_, token))
            when Token.is_int token ->
              fail region Orphan_minus
        | _ -> fail region Unterminated_integer }

| '-' "0x" byte_seq?
      { let region, _, _ = sync state lexbuf
        in fail region Negative_byte_sequence }

| _ as c { let region, _, _ = sync state lexbuf
           in fail region (Unexpected_character c) }
 *)

(* Finishing a string *)

and scan_string thread state = parse
  nl         { fail thread#opening Broken_string }
| eof        { fail thread#opening Unterminated_string }
| ['\t' '\r' '\b']
             { let region, _, _ = sync state lexbuf
               in fail region Invalid_character_in_string }
| '"'        { let _, _, state = sync state lexbuf
               in push_char '"' thread, state }
| esc        { let _, lexeme, state = sync state lexbuf
               in scan_string (push_string lexeme thread) state lexbuf }
| '\\' _     { let region, _, _ = sync state lexbuf
               in fail region Undefined_escape_sequence }
| _ as c     { let _, _, state = sync state lexbuf in
               scan_string (push_char c thread) state lexbuf }

(* Finishing a block comment

   The lexing of block comments must take care of embedded block
   comments that may occur within, as well as strings, so no substring
   "*/" may inadvertantly close the block. This is the purpose of the
   first case of the scanner [scan_block].
*)

and scan_block thread state = parse
  '"' | "/*" { let opening = thread#opening in
               let opening', lexeme, state = state#sync lexbuf in
               let thread = thread#push_string lexeme in
               let thread = thread#set_opening opening' in
               let next   = if lexeme = "\"" then scan_string
                            else scan_block in
               let thread, state = next thread state lexbuf in
               let thread = thread#set_opening opening
               in scan_block thread state lexbuf }
| "*/"       { let _, lexeme, state = state#sync lexbuf
               in thread#push_string lexeme, state }
| nl as nl   { let ()     = Lexing.new_line lexbuf
               and state  = state#set_pos (state#pos#new_line nl)
               and thread = thread#push_string nl in
               scan_block thread state lexbuf }
| eof        { fail thread#opening Unterminated_comment }
| _          { let     () = LexerLib.rollback lexbuf in
               let len    = thread#length in
               let thread,
                   status = scan_utf8 thread state lexbuf in
               let delta  = thread#length - len in
               let pos    = state.pos#shift_one_uchar delta in
               match status with
                 Stdlib.Ok () ->
                   scan_block thread (state#set_pos pos) lexbuf
               | Error error ->
                   let region = Region.make ~start:state#pos ~stop:pos
                   in fail region error }

and scan_utf8 thread state = parse
     eof { fail thread#opening Unterminated_comment }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _ -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8 thread state lexbuf
           | `End         -> assert false }

(* Finishing a line comment *)

and scan_line thread state = parse
  nl as nl { let ()     = Lexing.new_line lexbuf
             and thread = thread#push_string nl
             and state  = state#set_pos (state#pos#new_line nl)
             in thread, state }
| eof      { thread, state }
| _        { let ()     = LexerLib.rollback lexbuf in
             let len    = thread#length in
             let thread,
                 status = scan_utf8_inline thread state lexbuf in
             let delta  = thread#length - len in
             let pos    = state#pos#shift_one_uchar delta in
             match status with
               Stdlib.Ok () ->
                 scan_line thread (state#set_pos pos) lexbuf
             | Error error ->
                 let region = Region.make ~start:state#pos ~stop:pos
                 in fail region error }

and scan_utf8_inline thread state = parse
     eof { thread, Stdlib.Ok () }
| _ as c { let thread = thread#push_char c in
           let lexeme = Lexing.lexeme lexbuf in
           let () = state#supply (Bytes.of_string lexeme) 0 1 in
           match Uutf.decode state#decoder with
             `Uchar _     -> thread, Stdlib.Ok ()
           | `Malformed _ -> thread, Stdlib.Error Invalid_utf8_sequence
           | `Await       -> scan_utf8_inline thread state lexbuf
           | `End         -> assert false }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let scan =
  let first_call = ref true in
  fun state lexbuf ->
    if   !first_call
    then (first_call := false; init state lexbuf)
    else scan state lexbuf

end (* of functor [Make] in HEADER *)

(* END TRAILER *)
}
