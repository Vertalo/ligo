(* Driver for the ReasonLIGO parser *)

(* Vendor dependencies *)

module Region     = Simple_utils.Region
module LexerLib   = Simple_utils.LexerLib

(* Internal dependencies *)

module EvalOpt    = Lexer_shared.EvalOpt
module LexToken   = Lexer_reasonligo.LexToken
module CST        = Cst.Reasonligo
module ParserUnit = Parser_shared.ParserUnit
module Pretty     = Parser_reasonligo.Pretty

(* Input/Output *)

module IO =
  struct
    let options =
      let block = LexerLib.mk_block ~opening:"/*" ~closing:"*/"
      in EvalOpt.read ~block ~line:"//" ".religo"
  end

module SubIO =
  struct
    type options = <
      libs    : string list;
      verbose : Set.Make (String).t;
      offsets : bool;
      block   : LexerLib.block_comment option;
      line    : LexerLib.line_comment option;
      ext     : string;
      mode    : [`Byte | `Point];
      cmd     : EvalOpt.command;
      mono    : bool;
      pretty  : bool
    >

    let options : options =
      object
        method libs    = IO.options#libs
        method verbose = IO.options#verbose
        method offsets = IO.options#offsets
        method block   = IO.options#block
        method line    = IO.options#line
        method ext     = IO.options#ext
        method mode    = IO.options#mode
        method cmd     = IO.options#cmd
        method mono    = IO.options#mono
        method pretty  = IO.options#pretty
      end

    let make =
      EvalOpt.make ~libs:options#libs
                   ~verbose:options#verbose
                   ~offsets:options#offsets
                   ?block:options#block
                   ?line:options#line
                   ~ext:options#ext
                   ~mode:options#mode
                   ~cmd:options#cmd
                   ~mono:options#mono
                   ~pretty:options#pretty
  end

module Parser =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Parser_reasonligo.Parser
  end

module ParserLog =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Cst_reasonligo.ParserLog
  end

module Lexer = Lexer_shared.Lexer.Make (LexToken)

module Unit =
  ParserUnit.Make (Lexer)(CST)(Parser)(Parser_msg)(ParserLog)(SubIO)

(* Main *)

let wrap = function
  Stdlib.Ok ast ->
    if IO.options#pretty then
      begin
        let doc = Pretty.print ast in
        let width =
          match Terminal_size.get_columns () with
            None -> 60
          | Some c -> c in
        PPrint.ToChannel.pretty 1.0 width stdout doc;
        print_newline ()
      end;
    flush_all ()
| Error msg ->
    (flush_all (); Printf.eprintf "\027[31m%s\027[0m%!" msg.Region.value)

let () =
  match IO.options#input with
    None -> Unit.contract_in_stdin () |> wrap
  | Some file_path -> Unit.contract_in_file file_path |> wrap
