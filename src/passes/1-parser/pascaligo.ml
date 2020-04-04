module AST = Parser_pascaligo.AST
module LexToken = Parser_pascaligo.LexToken
module Lexer = Lexer.Make (LexToken)
module Scoping = Parser_pascaligo.Scoping
module Region = Simple_utils.Region
module ParErr = Parser_pascaligo.ParErr
module SSet = Utils.String.Set

(* Mock IOs TODO: Fill them with CLI options *)

module type IO = sig
  val ext : string

  val options : EvalOpt.options
end

module PreIO = struct
  let ext = ".ligo"

  let pre_options =
    EvalOpt.make ~libs:[] ~verbose:SSet.empty ~offsets:true ~mode:`Point
      ~cmd:EvalOpt.Quiet ~mono:false
end

module Parser = struct
  type ast = AST.t

  type expr = AST.expr

  include Parser_pascaligo.Parser
end

module ParserLog = struct
  type ast = AST.t

  type expr = AST.expr

  include Parser_pascaligo.ParserLog
end

module PreUnit = ParserUnit.Make (Lexer) (AST) (Parser) (ParErr) (ParserLog)

module Errors = struct
  (* let data =
         [("location",
           fun () -> Format.asprintf "%a" Location.pp_lift @@ loc)] *)

  let generic message =
    let title () = "" and message () = message.Region.value in
    Trace.error ~data:[] title message
end

let parse (module IO : IO) parser =
  let module Unit = PreUnit (IO) in
  let local_fail error =
    Trace.fail
    @@ Errors.generic
    @@ Unit.format_error ~offsets:IO.options#offsets IO.options#mode error
  in
  match parser () with
  | Stdlib.Ok semantic_value -> Trace.ok semantic_value
  (* Lexing and parsing errors *)
  | Stdlib.Error error -> Trace.fail @@ Errors.generic error
  (* Scoping errors *)
  | exception Scoping.Error (Scoping.Reserved_name name) -> (
      let token = Lexer.Token.mk_ident name.Region.value name.Region.region in
      match token with
      | Stdlib.Error LexToken.Reserved_name ->
          Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
      | Ok invalid ->
          local_fail ("Reserved name.\nHint: Change the name.\n", None, invalid)
      )
  | exception Scoping.Error (Scoping.Duplicate_parameter name) -> (
      let token = Lexer.Token.mk_ident name.Region.value name.Region.region in
      match token with
      | Stdlib.Error LexToken.Reserved_name ->
          Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
      | Ok invalid ->
          local_fail
            ("Duplicate parameter.\nHint: Change the name.\n", None, invalid) )
  | exception Scoping.Error (Scoping.Duplicate_variant name) ->
      let token = Lexer.Token.mk_constr name.Region.value name.Region.region in
      local_fail
        ( "Duplicate constructor in this sum type declaration.\n\
           Hint: Change the constructor.\n",
          None,
          token )
  | exception Scoping.Error (Scoping.Non_linear_pattern var) -> (
      let token = Lexer.Token.mk_ident var.Region.value var.Region.region in
      match token with
      | Stdlib.Error LexToken.Reserved_name ->
          Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
      | Ok invalid ->
          local_fail
            ( "Repeated variable in this pattern.\nHint: Change the name.\n",
              None,
              invalid ) )
  | exception Scoping.Error (Scoping.Duplicate_field name) -> (
      let token = Lexer.Token.mk_ident name.Region.value name.Region.region in
      match token with
      | Stdlib.Error LexToken.Reserved_name ->
          Trace.fail @@ Errors.generic @@ Region.wrap_ghost "Reserved name."
      | Ok invalid ->
          local_fail
            ( "Duplicate field name in this record declaration.\n\
               Hint: Change the name.\n",
              None,
              invalid ) )

let parse_file source =
  let module IO = struct
    let ext = PreIO.ext

    let options = PreIO.pre_options ~input:(Some source) ~expr:false
  end in
  let module Unit = PreUnit (IO) in
  let lib_path =
    match IO.options#libs with
    | []   -> ""
    | libs ->
        let mk_I dir path = Printf.sprintf " -I %s%s" dir path in
        List.fold_right mk_I libs ""
  in
  let prefix =
    match IO.options#input with
    | None | Some "-" -> "temp"
    | Some file -> Filename.(remove_extension @@ basename file)
  in
  let suffix = ".pp" ^ IO.ext in
  let pp_input =
    if SSet.mem "cpp" IO.options#verbose then
      prefix ^ suffix
    else (
      let pp_input, pp_out = Filename.open_temp_file prefix suffix in
      close_out pp_out ; pp_input )
  in
  let cpp_cmd =
    match IO.options#input with
    | None | Some "-" ->
        Printf.sprintf "cpp -traditional-cpp%s - > %s" lib_path pp_input
    | Some file ->
        Printf.sprintf "cpp -traditional-cpp%s %s > %s" lib_path file pp_input
  in
  let open Trace in
  let%bind () = sys_command cpp_cmd in
  match Lexer.(open_token_stream @@ File pp_input) with
  | Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_contract in
      parse (module IO) thunk
  | Stdlib.Error (Lexer.File_opening msg) ->
      Trace.fail @@ Errors.generic @@ Region.wrap_ghost msg

let parse_string (s : string) =
  let module IO = struct
    let ext = PreIO.ext

    let options = PreIO.pre_options ~input:None ~expr:false
  end in
  let module Unit = PreUnit (IO) in
  match Lexer.(open_token_stream @@ String s) with
  | Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_contract in
      parse (module IO) thunk
  | Stdlib.Error (Lexer.File_opening msg) ->
      Trace.fail @@ Errors.generic @@ Region.wrap_ghost msg

let parse_expression (s : string) =
  let module IO = struct
    let ext = PreIO.ext

    let options = PreIO.pre_options ~input:None ~expr:true
  end in
  let module Unit = PreUnit (IO) in
  match Lexer.(open_token_stream @@ String s) with
  | Ok instance ->
      let thunk () = Unit.apply instance Unit.parse_expr in
      parse (module IO) thunk
  | Stdlib.Error (Lexer.File_opening msg) ->
      Trace.fail @@ Errors.generic @@ Region.wrap_ghost msg
