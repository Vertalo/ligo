(* Lexer specification for LIGO, to be processed by [ocamllex]. *)

{
(* START HEADER *)

[@@@warning "-42"]

(* VENDOR DEPENDENCIES *)

module Region   = Simple_utils.Region
module LexerLib = Simple_utils.LexerLib
module Markup   = Simple_utils.Markup

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

    type   int_err = Non_canonical_zero
    type ident_err = Reserved_name
    type   nat_err = Invalid_natural
                   | Non_canonical_zero_nat
    type   sym_err = Invalid_symbol
    type  attr_err = Invalid_attribute

    (* Injections *)

    val mk_int      : lexeme -> Region.t -> (token,   int_err) result
    val mk_nat      : lexeme -> Region.t -> (token,   nat_err) result
    val mk_mutez    : lexeme -> Region.t -> (token,   int_err) result
    val mk_ident    : lexeme -> Region.t -> (token, ident_err) result
    val mk_sym      : lexeme -> Region.t -> (token,   sym_err) result
    val mk_string   : lexeme -> Region.t -> token
    val mk_verbatim : lexeme -> Region.t -> token
    val mk_bytes    : lexeme -> Region.t -> token
    val mk_constr   : lexeme -> Region.t -> token
    val mk_attr     : string -> lexeme -> Region.t -> (token, attr_err) result
    val mk_lang     : lexeme Region.reg -> Region.t -> token
    val eof         : Region.t -> token

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
  end

module Make (Token : TOKEN) : (S with module Token = Token) =
  struct
    module Token = Token
    type token = Token.token

    (* ERRORS *)

    type error =
      Unexpected_character of char
    | Non_canonical_zero
    | Reserved_name of string
    | Invalid_symbol
    | Invalid_natural
    | Invalid_attribute
    | Unterminated_verbatim

    let sprintf = Printf.sprintf

    let error_to_string = function
      Unexpected_character c ->
        sprintf "Unexpected character '%s'." (Char.escaped c)
    | Non_canonical_zero ->
        "Non-canonical zero.\n\
         Hint: Use 0."
    | Reserved_name s ->
        sprintf "Reserved name: \"%s\".\n\
         Hint: Change the name." s
    | Invalid_symbol ->
        "Invalid symbol.\n\
         Hint: Check the LIGO syntax you use."
    | Invalid_natural ->
        "Invalid natural number."
    | Invalid_attribute ->
        "Invalid attribute."
    | Unterminated_verbatim ->
       "Unterminated verbatim.\n\
        Hint: Close with \"|}\"."

    exception Error of error Region.reg

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

    let mk_verbatim (thread, state) =
      let start  = thread#opening#start in
      let stop   = state#pos in
      let region = Region.make ~start ~stop in
      let lexeme = thread#to_string in
      let token  = Token.mk_verbatim lexeme region
      in state#enqueue token

    let mk_bytes bytes state buffer =
      let open LexerLib in
      let {region; state; _} = state#sync buffer in
      let token = Token.mk_bytes bytes region
      in state#enqueue token

    let mk_int state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_int lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_nat state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_nat lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero_nat ->
          fail region Non_canonical_zero
      | Error Token.Invalid_natural ->
          fail region Invalid_natural

    let mk_mutez state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_mutez lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let mk_tez state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      let lexeme = Z.mul (Z.of_int 1_000_000) (Z.of_string lexeme) in
      match Token.mk_mutez (Z.to_string lexeme ^ "mutez") region with
        Ok token -> state#enqueue token
      | Error Token.Non_canonical_zero ->
          fail region Non_canonical_zero

    let format_tez s =
      match String.index s '.' with
        index ->
          let len         = String.length s in
          let integral    = Str.first_chars s index
          and fractional  = Str.last_chars s (len-index-1) in
          let num         = Z.of_string (integral ^ fractional)
          and den         = Z.of_string ("1" ^ String.make (len-index-1) '0')
          and million     = Q.of_string "1000000" in
          let mutez       = Q.make num den |> Q.mul million in
          let should_be_1 = Q.den mutez in
          if Z.equal Z.one should_be_1 then Some (Q.num mutez) else None
      | exception Not_found -> assert false

    let mk_tez_decimal state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      let lexeme = Str.(global_replace (regexp "_") "" lexeme) in
      let lexeme = Str.string_before lexeme (String.index lexeme 't') in
      match format_tez lexeme with
        None -> assert false
      | Some tz ->
          match Token.mk_mutez (Z.to_string tz ^ "mutez") region with
            Ok token -> state#enqueue token
        | Error Token.Non_canonical_zero ->
            fail region Non_canonical_zero

    let mk_ident state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_ident lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Reserved_name ->
          fail region (Reserved_name lexeme)

    let mk_attr header attr state buffer =
      let open LexerLib in
      let {region; state; _} = state#sync buffer in
      match Token.mk_attr header attr region with
        Ok token -> state#enqueue token
      | Error Token.Invalid_attribute ->
          fail region Invalid_attribute

    let mk_constr state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      let token = Token.mk_constr lexeme region
      in state#enqueue token

    let mk_lang lang state buffer =
      let LexerLib.{region; state; _}
                             = state#sync buffer in
      let start              = region#start#shift_bytes 1 in
      let stop               = region#stop in
      let lang_reg           = Region.make ~start ~stop in
      let lang               = Region.{value=lang; region=lang_reg} in
      let token              = Token.mk_lang lang region
      in state#enqueue token

    let mk_sym state buffer =
      let open LexerLib in
      let {region; lexeme; state} = state#sync buffer in
      match Token.mk_sym lexeme region with
        Ok token -> state#enqueue token
      | Error Token.Invalid_symbol -> fail region Invalid_symbol

    let mk_eof state buffer =
      let open LexerLib in
      let {region; state; _} = state#sync buffer in
      let token = Token.eof region
      in state#enqueue token

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let nl         = ['\n' '\r'] | "\r\n"
let blank      = ' ' | '\t'
let digit      = ['0'-'9']
let natural    = digit | digit (digit | '_')* digit
let decimal    = natural '.' natural
let small      = ['a'-'z']
let capital    = ['A'-'Z']
let letter     = small | capital
let ident      = small (letter | '_' | digit)*
let constr     = capital (letter | '_' | digit)*
let attr       = ident | constr
let hexa_digit = digit | ['A'-'F' 'a'-'f']
let byte       = hexa_digit hexa_digit
let byte_seq   = byte | byte (byte | '_')* byte
let bytes      = "0x" (byte_seq? as seq)
let string     = [^'"' '\\' '\n']*  (* For strings of #include *)

(* Symbols *)

let common_sym     =   ';' | ',' | '(' | ')'  | '[' | ']'  | '{' | '}'
                     | '=' | ':' | '|' | "->" | '.' | '_'  | '^'
                     | '+' | '-' | '*' | '/'  | '<' | "<=" | '>' | ">="
let pascaligo_sym  = "=/=" | '#' | ":="
let cameligo_sym   = "<>" | "::" | "||" | "&&"
let reasonligo_sym = '!' | "=>" | "!=" | "==" | "++" | "..." | "||" | "&&"

let symbol = common_sym | pascaligo_sym | cameligo_sym | reasonligo_sym

(* RULES *)

(* The scanner [scan] has a parameter [state] that is thread through
   recursive calls. *)

rule scan state = parse
  ident                  { mk_ident        state lexbuf }
| constr                 { mk_constr       state lexbuf }
| bytes                  { mk_bytes seq    state lexbuf }
| natural 'n'            { mk_nat          state lexbuf }
| natural "mutez"        { mk_mutez        state lexbuf }
| natural "tz"
| natural "tez"          { mk_tez          state lexbuf }
| decimal "tz"
| decimal "tez"          { mk_tez_decimal  state lexbuf }
| natural                { mk_int          state lexbuf }
| symbol                 { mk_sym          state lexbuf }
| "[@"  (attr as a) "]"  { mk_attr "[@"  a state lexbuf }
| "[@@" (attr as a) "]"  { mk_attr "[@@" a state lexbuf }
| "[%"  (attr as l)      { mk_lang       l state lexbuf }

| "{|" {
    let LexerLib.{region; state; _} = state#sync lexbuf in
    let thread                      = LexerLib.mk_thread region
    in scan_verbatim thread state lexbuf |> mk_verbatim }

| _ as c { let LexerLib.{region; _} = state#sync lexbuf
           in fail region (Unexpected_character c) }

and scan_verbatim thread state = parse
  nl as nl { let ()    = Lexing.new_line lexbuf
             and state = state#set_pos (state#pos#new_line nl) in
             scan_verbatim (thread#push_string nl) state lexbuf }
| '#' blank* (natural as line) blank+ '"' (string as file) '"' {
             let state = LexerLib.line_preproc ~line ~file state lexbuf
             in scan_verbatim thread state lexbuf }
| eof      { fail thread#opening Unterminated_verbatim       }
| "|}"     { LexerLib.(thread, (state#sync lexbuf).state)    }
| _ as c   { let LexerLib.{state; _} = state#sync lexbuf in
             scan_verbatim (thread#push_char c) state lexbuf }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

let client = LexerLib.{mk_string; mk_eof; callback=scan}
let scan   = LexerLib.mk_scan client

end (* of functor [Make] in HEADER *)

(* END TRAILER *)
}
