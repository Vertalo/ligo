
module Errors = Errors
module I = Ast_imperative
module O = Ast_sugar
open Trace
open Stage_common.Passes

let rec decompile_type_expression : O.type_expression -> (I.type_expression, Errors.purification_error) result =
  fun te ->
  let self = decompile_type_expression in
  let return te = ok @@ I.make_t te in
  match te.type_content with
    | O.T_sum sum-> 
      (* This type sum could be a michelson_or as well, we could use is_michelson_or *)
      let%bind sum = row self sum in
      return @@ I.T_sum sum
    | O.T_record record -> 
      let%bind record = row self record in
      return @@ I.T_record record
    | O.T_tuple tuple ->
      let%bind tuple = bind_map_list self tuple in
      return @@ I.T_tuple tuple
    | O.T_arrow arr ->
      let%bind arr = arrow self arr in
      return @@ T_arrow arr
    | O.T_variable type_variable -> return @@ T_variable type_variable 
    | O.T_wildcard               -> return @@ T_wildcard
    | O.T_constant (type_constant, lst) ->
      let%bind lst = bind_map_list self lst in
      return @@ T_constant (type_constant, lst)

let rec decompile_expression : O.expression -> (I.expression, Errors.purification_error) result =
  fun e ->
  let self      = decompile_expression in
  let self_type = decompile_type_expression in
  let return expr = ok @@ I.make_e ~loc:e.location expr in
  match e.expression_content with 
    O.E_literal lit -> return @@ I.E_literal lit
  | O.E_variable name     -> return @@ I.E_variable name
  | O.E_constant cons -> 
    let%bind cons = constant self cons in
    return @@ I.E_constant cons
  | O.E_application app -> 
    let%bind app = application self app in
    return @@ I.E_application app
  | O.E_lambda lamb ->
    let%bind lamb = lambda self self_type lamb in
    return @@ I.E_lambda lamb
  | O.E_recursive recs ->
    let%bind recs = recursive self self_type recs in
    return @@ I.E_recursive recs
  | O.E_let_in {let_binder;inline;rhs;let_result} ->
    let%bind let_binder = binder self_type let_binder in
    let%bind rhs = self rhs in
    let%bind let_result = self let_result in
    return @@ I.E_let_in {let_binder;inline;rhs;let_result}
  | O.E_raw_code {language;code} ->
    let%bind code  = self code in
    return @@ I.E_raw_code {language;code} 
  | O.E_constructor {constructor;element} ->
    let%bind element = self element in
    return @@ I.E_constructor {constructor;element}
  | O.E_matching {matchee; cases} ->
    let%bind matchee = self matchee in
    let%bind cases   = decompile_matching cases in
    return @@ I.E_matching {matchee;cases}
  | O.E_record recd ->
    let%bind recd = record self recd in
    return @@ I.E_record recd
  | O.E_accessor acc ->
    let%bind acc = accessor self acc in
    return @@ I.E_accessor acc
  | O.E_update up ->
    let%bind up = update self up in
    return @@ I.E_update up
  | O.E_tuple tuple ->
    let%bind tuple = bind_map_list self tuple in
    return @@ I.E_tuple tuple
  | O.E_ascription ascr ->
    let%bind ascr = ascription self self_type ascr in
    return @@ I.E_ascription ascr
  | O.E_cond cond ->
    let%bind cond = conditional self cond in
    return @@ I.E_cond cond
  | O.E_sequence seq ->
    let%bind seq = sequence self seq in
    return @@ I.E_sequence seq
  | O.E_skip -> return @@ I.E_skip
  | O.E_map map ->
    let%bind map = bind_map_list (
      bind_map_pair self
    ) map
    in
    return @@ I.E_map map
  | O.E_big_map big_map ->
    let%bind big_map = bind_map_list (
      bind_map_pair self
    ) big_map
    in
    return @@ I.E_big_map big_map
  | O.E_list lst ->
    let%bind lst = bind_map_list self lst in
    return @@ I.E_list lst
  | O.E_set set ->
    let%bind set = bind_map_list self set in
    return @@ I.E_set set 

and decompile_matching : O.matching_expr -> (I.matching_expr, Errors.purification_error) result =
  fun m -> 
  match m with 
    | O.Match_list {match_nil;match_cons} ->
      let%bind match_nil = decompile_expression match_nil in
      let (hd,tl,expr) = match_cons in
      let%bind expr = decompile_expression expr in
      ok @@ I.Match_list {match_nil; match_cons=(hd,tl,expr)}
    | O.Match_option {match_none;match_some} ->
      let%bind match_none = decompile_expression match_none in
      let (n,expr) = match_some in
      let%bind expr = decompile_expression expr in
      ok @@ I.Match_option {match_none; match_some=(n,expr)}
    | O.Match_variant lst ->
      let%bind lst = bind_map_list (
        fun ((c,n),expr) ->
          let%bind expr = decompile_expression expr in
          ok @@ ((c,n),expr)
      ) lst 
      in
      ok @@ I.Match_variant lst
    | O.Match_record (lst,expr) ->
      let%bind expr = decompile_expression expr in
      let aux (a,b) = 
        let%bind b = binder decompile_type_expression b in
        ok @@ (a,b) in
      let%bind lst = bind_map_list aux lst in
      ok @@ I.Match_record (lst,expr)
    | O.Match_tuple (lst,expr) ->
      let%bind expr = decompile_expression expr in
      let%bind lst = bind_map_list (binder decompile_type_expression) lst in
      ok @@ I.Match_tuple (lst,expr)
    | O.Match_variable (b,expr) ->
      let%bind expr = decompile_expression expr in
      let%bind binder = binder decompile_type_expression b in
      ok @@ I.Match_variable (binder,expr)

let decompile_declaration : O.declaration Location.wrap -> _ result = fun {wrap_content=declaration;location} ->
  let return decl = ok @@ Location.wrap ~loc:location decl in
  match declaration with 
  | O.Declaration_constant (n, te, inline, expr) ->
    let%bind expr = decompile_expression expr in
    let%bind te   = decompile_type_expression te in
    return @@ I.Declaration_constant (n, te, inline, expr)
  | O.Declaration_type (n, te) ->
    let%bind te = decompile_type_expression te in
    return @@ I.Declaration_type (n,te)

let decompile_program : O.program -> (I.program, Errors.purification_error) result = fun prg ->
  bind_map_list decompile_declaration prg
