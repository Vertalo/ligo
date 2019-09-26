(* Driver for the parser of CameLIGO *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

(* Reading the command-line options *)

let options = EvalOpt.read ()

open EvalOpt

(* Path to the Mini-ML standard library *)

let lib_path =
  match options.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

(* Opening the input channel and setting the lexing engine *)

let cin, reset =
  match options.input with
    None | Some "-" -> stdin, ignore
  |       Some file -> open_in file, Lexer.reset_file ~file

let buffer = Lexing.from_channel cin
let     () = reset buffer

(* Tokeniser *)

let tokeniser =
  if Utils.String.Set.mem "lexer" options.verbose then
    Lexer.get_token ~log:(stdout, Lexer.output_token buffer)
  else Lexer.get_token ?log:None

let () =
  try
    let ast = Parser.program tokeniser buffer in
    if Utils.String.Set.mem "parser" options.verbose
    then AST.print_tokens ast
  with
    Lexer.Error diag ->
      close_in cin; Lexer.prerr ~kind:"Lexical" diag
  | Parser.Error ->
      let start  = Pos.from_byte (Lexing.lexeme_start_p buffer)
      and stop   = Pos.from_byte (Lexing.lexeme_end_p buffer) in
      let region = Region.make ~start ~stop in
      close_in cin;
      Lexer.prerr ~kind:"Syntactical"
                  Region.{value="Parse error."; region}
  | Sys_error msg -> Utils.highlight msg
