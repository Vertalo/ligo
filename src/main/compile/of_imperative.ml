open Trace
open Ast_imperative
open Imperative_to_sugar

type form = 
  | Contract of string
  | Env

let compile (program : program) : Ast_sugar.program result =
  let%bind program = compile_program program in
  Self_ast_sugar.all_program program

let compile_expression (e : expression) : Ast_sugar.expression result =
  let%bind e = compile_expression e in
  Self_ast_sugar.all_expression e

let pretty_print formatter (program : program) = 
  PP.program formatter program

let list_declarations (program : program) : string list =
  List.fold_left
    (fun prev el -> 
      let open Location in
      match el.wrap_content with
      | Declaration_constant (var,_,_,_) -> (Var.to_name var)::prev
      | _ -> prev) 
    [] program
