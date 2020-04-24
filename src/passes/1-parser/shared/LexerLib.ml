(* Sliding window *)

(* The type [window] models a two-slot buffer of tokens for reporting
   after a parse error. Technically, it is a parametric type, but its
   use is meant for tokens, wherever they are defined.

   In [Two(t1,t2)], the token [t2] is the next to be sent to the
   parser.

   The call [slide token buffer] pushes the token [token] in the
   buffer [buffer]. If the buffer is full, that is, it is
   [Two(t1,t2)], then the token [t2] is discarded to make room for
   [token].
 *)

type 'a window =
  Nil
| One of 'a
| Two of 'a * 'a

let slide token = function
  Nil -> One token
| One t | Two (t,_) -> Two (token,t)

module Region = Simple_utils.Region
module Pos = Simple_utils.Pos

type lexeme = string

(* The signature [TOKEN] exports an abstract type [token], so a lexer
   can be a functor over tokens. This enables to externalise
   version-dependent constraints in any module whose signature matches
   [TOKEN]. Generic functions to construct tokens are required.

   Note the predicate [is_eof], which caracterises the virtual token
   for end-of-file, because it requires special handling.
*)

module type TOKEN =
  sig
    type token

    (* Predicates *)

    val is_eof : token -> bool

    (* Projections *)

    val to_lexeme : token -> lexeme
    val to_string : token -> ?offsets:bool -> [`Byte | `Point] -> string
    val to_region : token -> Region.t

    (* Style *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool ->
      [`Byte | `Point] ->
      error Region.reg ->
      file:bool ->
      string Region.reg

    val check_right_context :
      token ->
      (Lexing.lexbuf -> (Markup.t list * token) option) ->
      Lexing.lexbuf ->
      unit
  end

(* The module type for lexers is [S]. It mainly exports the function
   [open_token_stream], which returns

     * a function [read] that extracts tokens from a lexing buffer,
       together with a lexing buffer [buffer] to read from,
     * a function [close] that closes that buffer,
     * a function [get_pos] that returns the current position, and
     * a function [get_last] that returns the region of the last
       recognised token.
     * a function [get_file] that returns the name of the file being
       scanned (empty string if [stdin]).

   Note that a module [Token] is exported too, because the signature
   of the exported functions depend on it.

     The type [window] is a two-token window, that is, a buffer that
   contains the last recognised token, and the penultimate (if any).

     The call [read ~log] evaluates in a lexer (also known as a
   tokeniser or scanner) whose type is [Lexing.lexbuf -> token], and
   suitable for a parser generated by Menhir. The argument labelled
   [log] is a logger, that is, it may print a token and its left
   markup to a given channel, at the caller's discretion.
*)

module type S =
  sig
    module Token : TOKEN
    type token = Token.token

    type file_path = string
    type logger = Markup.t list -> token -> unit

    type input =
      File    of file_path
    | String  of string
    | Channel of in_channel
    | Buffer  of Lexing.lexbuf

    type instance = {
      input    : input;
      read     : log:logger -> Lexing.lexbuf -> token;
      buffer   : Lexing.lexbuf;
      get_win  : unit -> token window;
      get_pos  : unit -> Pos.t;
      get_last : unit -> Region.t;
      get_file : unit -> file_path;
      close    : unit -> unit
    }

    type open_err = File_opening of string

    val lexbuf_from_input :
      input -> (Lexing.lexbuf * (unit -> unit), open_err) Stdlib.result

    type line_comment = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    val mk_block : opening:string -> closing:string -> block_comment

    val open_token_stream :
      ?line:line_comment ->
      ?block:block_comment ->
      input ->
      (instance, open_err) Stdlib.result

    (* Error reporting *)

    type error

    val error_to_string : error -> string

    exception Error of error Region.reg

    val format_error :
      ?offsets:bool ->
      [`Byte | `Point] ->
      error Region.reg ->
      file:bool ->
      string Region.reg
  end

(* LEXER ENGINE *)

(* Resetting file name and line number in the lexing buffer

   The call [reset ~file ~line buffer] modifies in-place the lexing
   buffer [buffer] so the lexing engine records that the file
   associated with [buffer] is named [file], and the current line is
   [line]. This function is useful when lexing a file that has been
   previously preprocessed by the C preprocessor, in which case the
   argument [file] is the name of the file that was preprocessed,
   _not_ the preprocessed file (of which the user is not normally
   aware). By default, the [line] argument is [1].
 *)

let reset_file ~file buffer =
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_fname = file}

let reset_line ~line buffer =
  assert (line >= 0);
  let open Lexing in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_lnum = line}

let reset_offset ~offset buffer =
  assert (offset >= 0);
  let open Lexing in
  let bol = buffer.lex_curr_p.pos_bol in
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum = bol + offset }

let reset ?file ?line ?offset buffer =
  let () =
    match file with
      Some file -> reset_file ~file buffer
    |      None -> () in
  let () =
    match line with
      Some line -> reset_line ~line buffer
    |      None -> () in
  match offset with
    Some offset -> reset_offset ~offset buffer
  |        None -> ()

(* Rolling back one lexeme _within the current semantic action_ *)

let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}

(* THREAD FOR STRUCTURED CONSTRUCTS (STRINGS, COMMENTS) *)

(* When scanning structured constructs, like strings and comments, we
   need to keep the region of the opening symbol (like double quote,
   "//" or "(*") in order to report any error more precisely. Since
   ocamllex is byte-oriented, we need to store the parsed bytes as
   characters in an accumulator [acc] and also its length [len], so,
   we are done, it is easy to build the string making up the
   structured construct with [mk_str] (see above).

     The resulting data structure is called a _thread_. (Note for
   Emacs: "*)".)
*)

type thread = <
  opening : Region.t;
  length  : int;
  acc     : char list;
  push_char : char -> thread;
  push_string : string -> thread;
  to_string : string;
  set_opening : Region.t -> thread
>

let mk_thread region lexeme : thread =
  (* The call [explode s a] is the list made by pushing the characters
     in the string [s] on top of [a], in reverse order. For example,
     [explode "ba" ['c';'d'] = ['a'; 'b'; 'c'; 'd']]. *)
  let explode s acc =
    let rec push = function
      0 -> acc
    | i -> s.[i-1] :: push (i-1)
    in push (String.length s)
  in
  object
    val opening = region
    method opening = opening

    val length = String.length lexeme
    method length = length

    val acc = explode lexeme []
    method acc = acc

    method set_opening opening = {< opening; length; acc >}

    method push_char char =
      {< opening; length=length+1; acc=char::acc >}

    method push_string str =
      {< opening;
         length = length + String.length str;
         acc = explode str acc >}

    (* The value of [thread#to_string] is a string of length
       [thread#length] containing the [thread#length] characters in
       the list [thread#acc], in reverse order. For instance,
       [thread#to_string = "abc"] if [thread#length = 3] and
       [thread#acc = ['c';'b';'a']]. *)

    method to_string =
      let bytes = Bytes.make length ' ' in
      let rec fill i = function
             [] -> bytes
      | char::l -> Bytes.set bytes i char; fill (i-1) l
      in fill (length-1) acc |> Bytes.to_string
  end

type file_path = string

    (* STATE *)

    (* Beyond tokens, the result of lexing is a state. The type
       [state] represents the logical state of the lexing engine, that
       is, a value which is threaded during scanning and which denotes
       useful, high-level information beyond what the type
       [Lexing.lexbuf] in the standard library already provides for
       all generic lexers.

         Tokens are the smallest units used by the parser to build the
       abstract syntax tree. The state includes a queue of recognised
       tokens, with the markup at the left of its lexeme until either
       the start of the file or the end of the previously recognised
       token.

         The markup from the last recognised token or, if the first
       token has not been recognised yet, from the beginning of the
       file is stored in the field [markup] of the state because it is
       a side-effect, with respect to the output token list, and we
       use a record with a single field [units] because that record
       may be easily extended during the future maintenance of this
       lexer.

         The state also includes a field [pos] which holds the current
       position in the LIGO source file. The position is not always
       updated after a single character has been matched: that depends
       on the regular expression that matched the lexing buffer.

         The field [window] is a two-token window, that is, a buffer
       that contains the last recognised token, and the penultimate
       (if any).

         The fields [decoder] and [supply] offer the support needed
       for the lexing of UTF-8 encoded characters in comments (the
       only place where they are allowed in LIGO). The former is the
       decoder proper and the latter is the effectful function
       [supply] that takes a byte, a start index and a length and feed
       it to [decoder]. See the documentation of the third-party
       library Uutf.
     *)

    type line_comment  = string (* Opening of a line comment *)
    type block_comment = <opening : string; closing : string>

    let mk_block ~opening ~closing : block_comment =
      object
        method opening = opening
        method closing = closing
      end

    type 'a state = <
      units   : (Markup.t list * 'a) FQueue.t;
      markup  : Markup.t list;
      window  : 'a window;
      last    : Region.t;
      pos     : Pos.t;
      decoder : Uutf.decoder;
      supply  : Bytes.t -> int -> int -> unit;
      block   : block_comment option;
      line    : line_comment option;

      enqueue      : 'a -> 'a state;
      set_units    : (Markup.t list * 'a) FQueue.t -> 'a state;
      set_last     : Region.t -> 'a state;
      set_pos      : Pos.t -> 'a state;
      slide_token  : 'a -> 'a state;

      sync         : Lexing.lexbuf -> Region.t * lexeme * 'a state;

      push_newline : Lexing.lexbuf -> 'a state;
      push_line    : thread -> 'a state;
      push_block   : thread -> 'a state;
      push_space   : Lexing.lexbuf -> 'a state;
      push_tabs    : Lexing.lexbuf -> 'a state;
      push_bom     : Lexing.lexbuf -> 'a state;
      push_markup  : Markup.t -> 'a state;
    >

    let mk_state ~units ~markup ~window ~last ~pos ~decoder ~supply
                 ?block ?line () : _ state =
      object (self)
        val units = units
        method units = units
        val markup = markup
        method markup = markup
        val window = window
        method window = window
        val last = last
        method last = last
        val pos = pos
        method pos = pos
        method decoder = decoder
        method supply = supply
        method block = block
        method line = line

        (* The call [enqueue (token, state)] updates functionally the
           state [state] by associating the token [token] with the
           stored markup and enqueuing the pair into the units
           queue. The field [markup] is then reset to the empty
           list. *)

        method enqueue token =
          {< units  = FQueue.enq (markup, token) units;
             markup = [] >}

        method set_units units = {< units = units >}
        method set_last region = {< last = region >}
        method set_pos pos = {< pos = pos >}

        method slide_token token =
          {< window = slide token window >}

        (* The call [sync state buffer] updates the current position
           in accordance with the contents of the lexing buffer, more
           precisely, depending on the length of the string which has
           just been recognised by the scanner: that length is used as
           a positive offset to the current column. *)

        method sync buffer =
          let lex   = Lexing.lexeme buffer in
          let len   = String.length lex in
          let start = pos in
          let stop  = start#shift_bytes len in
          let state = {< pos = stop >}
          in Region.make ~start ~stop, lex, state

        (* MARKUP *)

        (* Committing markup to the current logical state *)

        method push_markup unit = {< markup = unit :: markup >}

        method push_newline buffer =
          let ()     = Lexing.new_line buffer in
          let value  = Lexing.lexeme buffer in
          let start  = self#pos in
          let stop   = start#new_line value in
          let region = Region.make ~start ~stop in
          let unit   = Markup.Newline Region.{region; value}
          in {< pos = stop; markup = unit::markup >}

        method push_line thread =
          let start  = thread#opening#start in
          let region = Region.make ~start ~stop:self#pos
          and value  = thread#to_string in
          let unit   = Markup.LineCom Region.{region; value}
          in {< markup = unit::markup >}

        method push_block thread =
          let start  = thread#opening#start in
          let region = Region.make ~start ~stop:self#pos
          and value  = thread#to_string in
          let unit   = Markup.BlockCom Region.{region; value}
          in {< markup = unit::markup >}

        method push_space buffer =
          let region, lex, state = self#sync buffer in
          let value  = String.length lex in
          let unit   = Markup.Space Region.{region; value}
          in state#push_markup unit

        method push_tabs buffer =
          let region, lex, state = self#sync buffer in
          let value  = String.length lex in
          let unit   = Markup.Tabs Region.{region; value}
          in state#push_markup unit

        method push_bom buffer =
          let region, value, state = self#sync buffer in
          let unit = Markup.BOM Region.{region; value}
          in state#push_markup unit

      end
