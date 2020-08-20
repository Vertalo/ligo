(* Driver for the Michelson lexer *)

(* Vendor dependencies *)

module Region    = Simple_utils.Region
module LexerLib  = Simple_utils.LexerLib

(* Internal dependencies *)

module EvalOpt   = Lexer_shared.EvalOpt
module Lexer     = Lexer_shared.Lexer
module LexerUnit = Lexer_shared.LexerUnit

(* Input/Output *)

module IO =
  struct
    let options =
      let block = LexerLib.mk_block ~opening:"/*" ~closing:"*/"
      in EvalOpt.read ~block ~line:"#" ".tz"
  end

(* Instantiating the standalone lexer *)

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

(* Tracing all tokens in the source *)

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Printf.eprintf "\027[31m%s\027[0m%!" value
