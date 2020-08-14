module I = Ast_sugar
module O = Ast_core

open Errors
open Trace
open Stage_common.Passes

let cast_var = Location.map Var.todo_cast

let rec compile_type_expression : I.type_expression -> (O.type_expression , desugaring_error) result =
  fun te ->
  let self = compile_type_expression in
  let return tc = ok @@ O.make_t ~loc:te.location ~sugar:te tc in
  match te.type_content with
    | I.T_sum sum -> 
      let%bind sum = row self sum in
      return @@ O.T_sum sum
    | I.T_record record -> 
      let%bind record = row self record in
      return @@ O.T_record record
    | I.T_tuple tuple ->
      let aux (i,acc) el = 
        let%bind el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), ({associated_type=el;michelson_annotation=None;decl_pos=0}:_ O.row_element))::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) tuple in
      let record = O.LMap.of_list lst in
      return @@ O.T_record record
    | I.T_arrow {type1;type2} ->
      let%bind type1 = self type1 in
      let%bind type2 = self type2 in
      return @@ T_arrow {type1;type2}
    | I.T_variable type_variable -> return @@ T_variable (Var.todo_cast type_variable)
    | I.T_wildcard -> return @@ T_wildcard
    | I.T_constant (type_constant, arguments) ->
      let%bind arguments = bind_map_list self arguments in
      return @@ T_constant (type_constant, arguments)

let rec compile_expression : I.expression -> (O.expression , desugaring_error) result =
  fun sugar ->
  let self = compile_expression in
  let self_type = compile_type_expression in
  let return expr = ok @@ O.make_e ~loc:sugar.location ~sugar expr in
  match sugar.expression_content with
    | I.E_literal literal -> return @@ O.E_literal literal
    | I.E_constant {cons_name;arguments} -> 
      let cons_name = cons_name in
      let%bind arguments = bind_map_list self arguments in
      return @@ O.E_constant {cons_name;arguments}
    | I.E_variable name -> return @@ O.E_variable (cast_var name)
    | I.E_application {lamb;args} -> 
      let%bind lamb = self lamb in
      let%bind args = self args in
      return @@ O.E_application {lamb; args}
    | I.E_lambda lamb ->
      let%bind lamb = lambda self self_type lamb in
      return @@ O.E_lambda lamb
    | I.E_recursive recs ->
      let%bind recs = recursive self self_type recs in
      return @@ O.E_recursive recs
    | I.E_let_in {let_binder;inline;rhs;let_result} ->
      let%bind let_binder = binder self_type let_binder in
      let%bind rhs = self rhs in
      let%bind let_result = self let_result in
      return @@ O.E_let_in {let_binder;inline;rhs;let_result}
    | I.E_raw_code {language;code} ->
      let%bind code = self code in
      return @@ O.E_raw_code {language;code} 
    | I.E_constructor {constructor;element} ->
      let constructor = constructor in
      let%bind element = self element in
      return @@ O.E_constructor {constructor;element}
    | I.E_matching {matchee; cases} ->
      let%bind matchee = self matchee in
      compile_matching sugar matchee cases
    | I.E_record record ->
      let record = I.LMap.to_kv_list record in
      let%bind record = 
        bind_map_list (fun (I.Label k,v) ->
          let%bind v =self v in
          ok @@ (O.Label k,v)
        ) record
      in
      return @@ O.E_record (O.LMap.of_list record)
    | I.E_accessor {record;path} ->
      let%bind record = self record in
      let accessor ?loc expr a =
        match a with 
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k -> 
          let%bind k = self k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      bind_fold_list accessor record path
    | I.E_update {record;path;update} ->
      let%bind record = self record in
      let%bind update = self update in
      let accessor ?loc expr a =
        match a with 
          I.Access_tuple  i -> ok @@ O.e_record_accessor ?loc expr (Label (Z.to_string i))
        | I.Access_record a -> ok @@ O.e_record_accessor ?loc expr (Label a)
        | I.Access_map k -> 
          let%bind k = self k in
          ok @@ O.e_constant ?loc C_MAP_FIND_OPT [k;expr]
      in
      let updator ?loc (s:O.expression) a expr =
        match a with
          I.Access_tuple  i -> ok @@ O.e_record_update ?loc s (Label (Z.to_string i)) expr
        | I.Access_record a -> ok @@ O.e_record_update ?loc s (Label a) expr
        | I.Access_map k ->
          let%bind k = self k in
          ok @@ O.e_constant ?loc C_MAP_ADD [k;expr;s]
      in
      let aux (s, e : O.expression * _) lst =
        let%bind s' = accessor ~loc:s.location s lst in
        let e' = fun expr -> 
          let%bind u = updator ~loc:s.location s lst (expr)
          in e u 
        in
        ok @@ (s',e')
      in
      let%bind (_,rhs) = bind_fold_list aux (record, fun e -> ok @@ e) path in
      rhs @@ update
    | I.E_map map -> (
      let map = List.sort_uniq compare map in
      let aux = fun prev (k, v) ->
        let%bind (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init map
    )
    | I.E_big_map big_map -> (
      let big_map = List.sort_uniq compare big_map in
      let aux = fun prev (k, v) ->
        let%bind (k', v') = bind_map_pair (self) (k, v) in
        return @@ E_constant {cons_name=C_MAP_ADD;arguments=[k' ; v' ; prev]}
      in
      let%bind init = return @@ E_constant {cons_name=C_BIG_MAP_EMPTY;arguments=[]} in
      bind_fold_right_list aux init big_map
    )
    | I.E_list lst ->
      let%bind lst' = bind_map_list (self) lst in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_CONS;arguments=[cur ; prev]} in
      let%bind init  = return @@ E_constant {cons_name=C_LIST_EMPTY;arguments=[]} in
      bind_fold_right_list aux init lst'
    | I.E_set set -> (
      let%bind lst' = bind_map_list (self) set in
      let lst' = List.sort_uniq compare lst' in
      let aux = fun prev cur ->
        return @@ E_constant {cons_name=C_SET_ADD;arguments=[cur ; prev]} in
      let%bind init = return @@ E_constant {cons_name=C_SET_EMPTY;arguments=[]} in
      bind_fold_list aux init lst'
      )
    | I.E_ascription {anno_expr; type_annotation} ->
      let%bind anno_expr = self anno_expr in
      let%bind type_annotation = self_type type_annotation in
      return @@ O.E_ascription {anno_expr; type_annotation}
    | I.E_cond {condition; then_clause; else_clause} ->
      let%bind matchee = self condition in
      let%bind match_true = self then_clause in
      let%bind match_false = self else_clause in
      let muted = Location.wrap @@ Var.of_name "_" in
      return @@ O.E_matching {
        matchee ;
        cases = Match_variant ([
          {constructor=Label "true"; proj=muted; body=match_true} ;
          {constructor=Label "false"; proj=muted; body=match_false} ;
        ])}
    | I.E_sequence {expr1; expr2} ->
      let%bind expr1 = self expr1 in
      let%bind expr2 = self expr2 in
      let let_binder : _ O.binder = {var = Location.wrap @@ Var.of_name "_" ; ty = O.t_unit ()} in
      return @@ O.E_let_in {let_binder; rhs=expr1;let_result=expr2; inline=false}
    | I.E_skip -> ok @@ O.e_unit ~loc:sugar.location ~sugar ()
    | I.E_tuple t ->
      let aux (i,acc) el = 
        let%bind el = self el in
        ok @@ (i+1,(O.Label (string_of_int i), el)::acc) in
      let%bind (_, lst ) = bind_fold_list aux (0,[]) t in
      let m = O.LMap.of_list lst in
      return @@ O.E_record m

and compile_matching : I.expression -> O.expression -> I.matching_expr -> (O.expression, desugaring_error) result =
  fun sugar e m -> 
  let loc = sugar.location in
  match m with 
    | I.Match_list {match_nil;match_cons} ->
      let%bind match_nil = compile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let hd = cast_var hd in
      let tl = cast_var tl in
      let%bind body = compile_expression expr in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_list {match_nil; match_cons={hd;tl;body}}
    | I.Match_option {match_none;match_some} ->
      let%bind match_none = compile_expression match_none in
      let (opt,body) = match_some in
      let opt = cast_var opt in
      let%bind body = compile_expression body in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_option {match_none; match_some={opt;body}}
    | I.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,proj),expr) ->
          let%bind body = compile_expression expr in
          ok @@ ({constructor=c;proj=cast_var proj;body} : O.match_variant)
      ) lst 
      in
      ok @@ O.e_matching ~loc ~sugar e @@ O.Match_variant lst
    | I.Match_record (binders, expr) ->
      let%bind next   = compile_expression expr in
      let aux ((index,expr) : int * _ ) (((I.Label field),b): (I.label * I.ty_expr I.binder)) =
        let%bind binders = binder compile_type_expression b in
        let f = O.e_let_in ~sugar binders false (O.e_record_accessor ~sugar e (O.Label field)) in
        ok @@ (index + 1, fun e -> expr @@ f e) 
      in
      let%bind (_,expr) = bind_fold_list aux (0, fun e -> e) @@ binders
      in
      ok @@ expr next
    | I.Match_tuple (binders, expr) ->
      let%bind next = compile_expression expr in
      let aux ((index,expr) : int * _ ) b = 
        let%bind binder = binder compile_type_expression b in
        let f = O.e_let_in ~sugar binder false (O.e_record_accessor ~sugar e (Label (string_of_int index))) in
        ok @@ (index + 1, fun e -> expr @@ f e) 
      in
      let%bind (_,expr) = bind_fold_list aux (0, fun e -> e) @@ binders
      in
      ok @@ expr next
    | I.Match_variable (b, expr) ->
      let%bind binder = binder compile_type_expression b in
      let%bind expr = compile_expression expr in
      ok @@ O.e_let_in ~sugar binder false e expr
 
let compile_declaration : I.declaration Location.wrap -> _ =
  fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | I.Declaration_constant (var, ty, inline, expr) ->
    let var = cast_var var in
    let%bind expr = compile_expression expr in
    let%bind ty = compile_type_expression ty in
    return @@ O.Declaration_constant {binder={var;ty}; attr={inline}; expr}
  | I.Declaration_type (type_binder, type_expr) ->
    let type_binder = Var.todo_cast type_binder in
    let%bind type_expr = compile_type_expression type_expr in
    return @@ O.Declaration_type {type_binder ; type_expr}

let compile_program : I.program -> (O.program , desugaring_error) result =
  fun p ->
  bind_map_list compile_declaration p
