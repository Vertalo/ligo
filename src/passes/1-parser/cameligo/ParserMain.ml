(** Driver for the CameLIGO parser *)

module IO =
  struct
    let ext = ".mligo"
    let options = EvalOpt.read "CameLIGO" ext
  end

module Parser =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include Parser
  end

module ParserLog =
  struct
    type ast  = CST.t
    type expr = CST.expr
    include ParserLog
  end

module Lexer = Lexer.Make (LexToken)

module Unit =
  ParserUnit.Make (Lexer)(CST)(Parser)(ParErr)(ParserLog)(IO)

(* Main *)

let issue_error error : ('a, string Region.reg) Stdlib.result =
  Stdlib.Error (Unit.format_error ~offsets:IO.options#offsets
                                  IO.options#mode error)

let parse parser : ('a, string Region.reg) Stdlib.result =
  try parser () with
    (* Scoping errors *)

  | Scoping.Error (Scoping.Reserved_name name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
          issue_error
            ("Reserved name.\nHint: Change the name.\n", None, invalid))

  | Scoping.Error (Scoping.Duplicate_variant name) ->
      let token =
        Lexer.Token.mk_constr name.Region.value name.Region.region in
      let point = "Duplicate constructor in this sum type declaration.\n\
                   Hint: Change the constructor.\n",
                  None, token
      in issue_error point

  | Scoping.Error (Scoping.Non_linear_pattern var) ->
      let token =
        Lexer.Token.mk_ident var.Region.value var.Region.region in
      (match token with
         (* Cannot fail because [var] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
           let point = "Repeated variable in this pattern.\n\
                        Hint: Change the name.\n",
                       None, invalid
           in issue_error point)

  | Scoping.Error (Scoping.Duplicate_field name) ->
      let token =
        Lexer.Token.mk_ident name.Region.value name.Region.region in
      (match token with
         (* Cannot fail because [name] is a not a
            reserved name for the lexer. *)
         Stdlib.Error _ -> assert false
       | Ok invalid ->
           let point = "Duplicate field name in this record declaration.\n\
                        Hint: Change the name.\n",
                       None, invalid
           in issue_error point)

(* Preprocessing the input source with CPP *)

module SSet = Utils.String.Set
let sprintf = Printf.sprintf

(* Path for CPP inclusions (#include) *)

let lib_path =
  match IO.options#libs with
    [] -> ""
  | libs -> let mk_I dir path = sprintf " -I %s%s" dir path
           in List.fold_right mk_I libs ""

let prefix =
  match IO.options#input with
    None | Some "-" -> "temp"
  | Some file -> Filename.(file |> basename |> remove_extension)

let suffix = ".pp" ^ IO.ext

let pp_input =
  if SSet.mem "cpp" IO.options#verbose
  then prefix ^ suffix
  else let pp_input, pp_out =
         Filename.open_temp_file prefix suffix
       in close_out pp_out; pp_input

let cpp_cmd =
  match IO.options#input with
    None | Some "-" ->
      sprintf "cpp -traditional-cpp%s - > %s"
              lib_path pp_input
  | Some file ->
      sprintf "cpp -traditional-cpp%s %s > %s"
              lib_path file pp_input

let () =
  if Sys.command cpp_cmd <> 0 then
    Printf.eprintf "External error: \"%s\" failed." cpp_cmd

(* Instantiating the lexer and calling the parser *)

let lexer_inst =
  match Lexer.open_token_stream (Lexer.File pp_input) with
    Ok instance ->
      if IO.options#expr
      then
        match parse (fun () -> Unit.apply instance Unit.parse_expr) with
          Stdlib.Ok _ -> ()
        | Error Region.{value; _} ->
           Printf.eprintf "\027[31m%s\027[0m%!" value
      else
        (match parse (fun () -> Unit.apply instance Unit.parse_contract) with
          Stdlib.Ok _ -> ()
        | Error Region.{value; _} ->
           Printf.eprintf "\027[31m%s\027[0m%!" value)
  | Stdlib.Error (Lexer.File_opening msg) ->
      Printf.eprintf "\027[31m%s\027[0m%!" msg
