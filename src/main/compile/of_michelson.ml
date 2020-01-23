open Tezos_utils
open Proto_alpha_utils
open Trace

module Errors = struct
(*
  TODO: those errors should have been caught in the earlier stages on the ligo pipeline
  build_contract is a kind of security net
*)
  let title_type_check_msg () = "generated Michelson contract failed to typecheck"
  let bad_parameter c () =
    let message () =
      let code = Format.asprintf "%a" Michelson.pp c in
      "bad contract parameter type (some michelson types are forbidden as contract parameter):\n"^code in
    error title_type_check_msg message
  let bad_storage c () =
    let message () =
      let code = Format.asprintf "%a" Michelson.pp c in
      "bad storage type (some michelson types are forbidden as contract storage):\n"^code in
    error title_type_check_msg message
  let bad_contract c () =
    let message () =
      let code = Format.asprintf "%a" Michelson.pp c in
      "bad contract type\n"^code in
    error title_type_check_msg message
  let unknown () =
    let message () =
      "unknown error" in
    error title_type_check_msg message
end

let build_contract : Compiler.compiled_expression -> Michelson.michelson result =
  fun compiled ->
  let%bind ((Ex_ty _param_ty),(Ex_ty _storage_ty)) = Self_michelson.fetch_contract_inputs compiled.expr_ty in
  let%bind param_michelson =
    Trace.trace_tzresult_lwt (simple_error "Could not unparse parameter") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _param_ty in
  let%bind storage_michelson =
    Trace.trace_tzresult_lwt (simple_error "Could not unparse storage") @@
    Proto_alpha_utils.Memory_proto_alpha.unparse_ty_michelson _storage_ty in
  let contract = Michelson.contract param_michelson storage_michelson compiled.expr in
  let%bind res =
    Trace.trace_tzresult_lwt (simple_error "Could not typecheck the code") @@
    Proto_alpha_utils.Memory_proto_alpha.typecheck_contract contract in
  match res with
  | Type_checked  -> ok contract
  | Err_parameter -> fail @@ Errors.bad_parameter contract ()
  | Err_storage   -> fail @@ Errors.bad_storage contract ()
  | Err_contract  -> fail @@ Errors.bad_contract contract ()
  | Err_unknown   -> fail @@ Errors.unknown ()

type check_type = Check_parameter | Check_storage
let assert_equal_contract_type : check_type -> Compiler.compiled_expression -> Compiler.compiled_expression -> unit result =
  fun c compiled_prg compiled_param ->
    let%bind (Ex_ty expected_ty) =
      let%bind (c_param_ty,c_storage_ty) = Self_michelson.fetch_contract_inputs compiled_prg.expr_ty in
      match c with
      | Check_parameter -> ok c_param_ty
      | Check_storage -> ok c_storage_ty in
    let (Ex_ty actual_ty) = compiled_param.expr_ty in
    let%bind _ = 
      Trace.trace_tzresult (simple_error "Passed parameter does not match the contract type") @@
      Proto_alpha_utils.Memory_proto_alpha.assert_equal_michelson_type expected_ty actual_ty in
    ok ()
