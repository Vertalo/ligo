(* Driver for the lexer of Ligodity *)

(* Error printing and exception tracing *)

let () = Printexc.record_backtrace true

(* Running the lexer on the source *)

let options = EvalOpt.read "CameLIGO" ".mligo"

open EvalOpt

let external_ text =
  Utils.highlight (Printf.sprintf "External error: %s" text); exit 1;;

(* Path for CPP inclusions (#include) *)

let lib_path =
  match options.libs with
      [] -> ""
  | libs -> let mk_I dir path = Printf.sprintf " -I %s%s" dir path
            in List.fold_right mk_I libs ""

(* Preprocessing the input source and opening the input channels *)

let prefix =
  match options.input with
    None | Some "-" -> "temp"
  | Some file ->  Filename.(file |> basename |> remove_extension)

let suffix = ".pp.mligo"

let pp_input =
  if Utils.String.Set.mem "cpp" options.verbose
  then prefix ^ suffix
  else let pp_input, pp_out = Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match options.input with
    None | Some "-" ->
      Printf.sprintf "cpp -traditional-cpp%s - > %s"
                     lib_path pp_input
  | Some file ->
      Printf.sprintf "cpp -traditional-cpp%s %s > %s"
                     lib_path file pp_input

let () =
  if Utils.String.Set.mem "cpp" options.verbose
  then Printf.eprintf "%s\n%!" cpp_cmd;
  if Sys.command cpp_cmd <> 0 then
    external_ (Printf.sprintf "the command \"%s\" failed." cpp_cmd)

(* Running the lexer on the input file *)

module Lexer = Lexer.Make (LexToken)

module Log = LexerLog.Make (Lexer)

let () = Log.trace ~offsets:options.offsets
           options.mode (Some pp_input) options.cmd
