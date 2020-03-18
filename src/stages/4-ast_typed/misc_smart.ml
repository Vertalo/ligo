open Trace
open Types
open Combinators
open Misc
open Stage_common.Types

let program_to_main : program -> string -> lambda result = fun p s ->
  let%bind (main , input_type , _) =
    let pred = fun d ->
      match d with
      | Declaration_constant (d , expr, _, _) when d = Var.of_name s -> Some expr
      | Declaration_constant _ -> None
    in
    let%bind main =
      trace_option (simple_error "no main with given name") @@
      List.find_map (Function.compose pred Location.unwrap) p in
    let%bind (input_ty , output_ty) =
      match (get_type' @@ get_type_expression main) with
      | T_arrow {type1;type2} -> ok (type1 , type2)
      | _ -> simple_fail "program main isn't a function" in
    ok (main , input_ty , output_ty)
  in
  let env =
    let aux = fun _ d ->
      match d with
      | Declaration_constant (_ , _, _, post_env) -> post_env in
    List.fold_left aux Environment.full_empty (List.map Location.unwrap p) in
  let binder = Var.of_name "@contract_input" in
  let result =
    let input_expr = e_a_variable binder input_type env in
    let main_expr = e_a_variable (Var.of_name s) (get_type_expression main) env in
    e_a_application main_expr input_expr env in
  ok {
    binder ;
    result ;
  }

module Captured_variables = struct

  type bindings = expression_variable list
  let mem : expression_variable -> bindings -> bool = List.mem
  let singleton : expression_variable -> bindings = fun s -> [ s ]
  let union : bindings -> bindings -> bindings = (@)
  let unions : bindings list -> bindings = List.concat
  let empty : bindings = []
  let of_list : expression_variable list -> bindings = fun x -> x

  let rec expression : bindings -> expression -> bindings result = fun b e ->
    expression_content b e.environment e.expression_content
  and expression_content : bindings -> full_environment -> expression_content -> bindings result = fun b env ec ->
    let self = expression b in
    match ec with
    | E_lambda l -> ok @@ Free_variables.lambda empty l
    | E_literal _ -> ok empty
    | E_constant {arguments;_} ->
      let%bind lst' = bind_map_list self arguments in
      ok @@ unions lst'
    | E_variable name -> (
        let%bind env_element =
          trace_option (simple_error "missing var in env") @@
          Environment.get_opt name env in
        match env_element.definition with
        | ED_binder -> ok empty
        | ED_declaration {expr=_ ; free_variables=_} -> simple_fail "todo"
      )
    | E_application {lamb;args} ->
      let%bind lst' = bind_map_list self [ lamb ; args ] in
      ok @@ unions lst'
    | E_constructor {element;_} -> self element
    | E_record m ->
      let%bind lst' = bind_map_list self @@ LMap.to_list m in
      ok @@ unions lst'
    | E_record_accessor {expr;_} -> self expr
    | E_record_update {record;update;_} -> 
      let%bind r = self record in
      let%bind e = self update in
      ok @@ union r e
    | E_list lst ->
      let%bind lst' = bind_map_list self lst in
      ok @@ unions lst'
    | E_set lst ->
      let%bind lst' = bind_map_list self lst in
      ok @@ unions lst'
    | (E_map m | E_big_map m) ->
      let%bind lst' = bind_map_list self @@ List.concat @@ List.map (fun (a, b) -> [ a ; b ]) m in
      ok @@ unions lst'
    | E_look_up (a , b) ->
      let%bind lst' = bind_map_list self [ a ; b ] in
      ok @@ unions lst'
    | E_matching {matchee;cases;_} ->
      let%bind a' = self matchee in
      let%bind cs' = matching_expression b cases in
      ok @@ union a' cs'
    | E_let_in li ->
      let b' = union (singleton li.let_binder) b in
      expression b' li.let_result
    | E_recursive r -> 
      let b' = union (singleton r.fun_name) b in
      expression_content b' env @@ E_lambda r.lambda

  and matching_variant_case : type a . (bindings -> a -> bindings result) -> bindings -> ((constructor' * expression_variable) * a) -> bindings result  = fun f b ((_,n),c) ->
    f (union (singleton n) b) c

  and matching : type a . (bindings -> a -> bindings result) -> bindings -> (a, 'tv) matching_content -> bindings result = fun f b m ->
    match m with
    | Match_bool { match_true = t ; match_false = fa } ->
      let%bind t' = f b t in
      let%bind fa' = f b fa in
      ok @@ union t' fa'
    | Match_list { match_nil = n ; match_cons = (hd, tl, c, _) } ->
      let%bind n' = f b n in
      let%bind c' = f (union (of_list [hd ; tl]) b) c in
      ok @@ union n' c'
    | Match_option { match_none = n ; match_some = (opt, s, _) } ->
      let%bind n' = f b n in
      let%bind s' = f (union (singleton opt) b) s in
      ok @@ union n' s'
    | Match_tuple ((lst , a),_) ->
      f (union (of_list lst) b) a
    | Match_variant (lst , _) ->
      let%bind lst' = bind_map_list (matching_variant_case f b) lst in
      ok @@ unions lst'

  and matching_expression = fun x -> matching expression x

end
