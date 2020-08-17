(* Parsing the command-line options of LIGO *)

(* Vendor dependencies *)

module LexerLib = Simple_utils.LexerLib

(* The type [command] denotes some possible behaviours of the
    compiler. The constructors are
      * [Quiet], then no output from the lexer and parser should be
        expected, safe error messages: this is the default value;

      * [Copy], then lexemes of tokens and markup will be printed to
        standard output, with the expectation of a perfect match with
        the input file;

      * [Units], then the tokens and markup will be printed to
        astandard output, that is, the abstract representation of the
        concrete lexical syntax;

      * [Tokens], then the tokens only will be printed.
 *)

type command = Quiet | Copy | Units | Tokens

(* The type [options] gathers the command-line options.
     * If the field [input] is [Some src], the name of the LIGO source
       file is [src]. If [input] is [Some "-"] or [None], the source
       file is read from standard input.

     * The field [libs] is the paths where to find LIGO files
       for inclusion (#include).

     * The field [verbose] is a set of stages of the compiler
       chain, about which more information may be displayed.

     * If the field [offsets] is [true], then the user requested
       that messages about source positions and regions be
       expressed in terms of horizontal offsets.

     * If the value [mode] is [`Byte], then the unit in which
       source positions and regions are expressed in messages is
       the byte. If [`Point], the unit is unicode points.

     * If the field [mono] is [true], then the monolithic API of
       Menhir is called, otherwise the incremental API is.

     * If the field [expr] is [true], then the parser for
       expressions is used, otherwise a full-fledged contract is
       expected.

     * If the field [pretty] is [true], then the source is
       pretty-printed on the standard out.
 *)

module SSet : Set.S with type elt = string and type t = Set.Make(String).t

type options = <
  input   : string option;
  libs    : string list;
  verbose : SSet.t;
  offsets : bool;
  block   : LexerLib.block_comment option;
  line    : LexerLib.line_comment option;
  ext     : string;
  mode    : [`Byte | `Point];
  cmd     : command;
  mono    : bool;
  expr    : bool;
  pretty  : bool
>

val make :
  input:string option ->
  libs:string list ->
  verbose:SSet.t ->
  offsets:bool ->
  ?block:LexerLib.block_comment ->
  ?line:LexerLib.line_comment ->
  ext:string ->
  mode:[`Byte | `Point] ->
  cmd:command ->
  mono:bool ->
  expr:bool ->
  pretty:bool ->
  options

(* Parsing the command-line options on stdin. *)

type extension = string

val read :
  ?block:LexerLib.block_comment ->
  ?line:LexerLib.line_comment ->
  extension ->
  options
