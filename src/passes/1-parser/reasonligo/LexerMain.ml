(* Driver for the ReasonLIGO lexer *)

module Region = Simple_utils.Region

module IO =
  struct
    let ext = ".religo"
    let options = EvalOpt.read "ReasonLIGO" ext
  end

module M = LexerUnit.Make (IO) (Lexer.Make (LexToken))

let () =
  match M.trace () with
    Stdlib.Ok () -> ()
  | Error Region.{value; _} -> Utils.highlight value
