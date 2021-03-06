open Trace

module I = Ast_simplified
module O = Ast_typed
open O.Combinators

module SMap = O.SMap

module Environment = O.Environment

type environment = Environment.t

module Errors = struct
  let unbound_type_variable (e:environment) (n:string) () =
    let title = (thunk "unbound type variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.full_environment e in
    error title full ()

  let unbound_variable (e:environment) (n:string) () =
    let title = (thunk "unbound variable") in
    let full () = Format.asprintf "%s in %a" n Environment.PP.full_environment e in
    error title full ()

  let unrecognized_constant (n:string) () =
    let title = (thunk "unrecognized constant") in
    let full () = n in
    error title full ()

  let wrong_arity (n:string) (expected:int) (actual:int) () =
    let title () = "wrong arity" in
    let full () =
      Format.asprintf "Wrong number of args passed to [%s]. Expected was %d, received was %d."
        n expected actual
    in
    error title full ()

  let program_error (p:I.program) () =
    let title = (thunk "typing program") in
    let full () = Format.asprintf "%a" I.PP.program p in
    error title full ()

  let constant_declaration_error (name:string) (ae:I.expr) () =
    let title = (thunk "typing constant declaration") in
    let full () =
      Format.asprintf "%s = %a" name
        I.PP.expression ae
    in
    error title full ()
end
open Errors

let rec type_program (p:I.program) : O.program result =
  let aux (e, acc:(environment * O.declaration Location.wrap list)) (d:I.declaration Location.wrap) =
    let%bind ed' = (bind_map_location (type_declaration e)) d in
    let loc : 'a . 'a Location.wrap -> _ -> _ = fun x v -> Location.wrap ~loc:x.location v in
    let (e', d') = Location.unwrap ed' in
    match d' with
    | None -> ok (e', acc)
    | Some d' -> ok (e', loc ed' d' :: acc)
  in
  let%bind (_, lst) =
    trace (fun () -> program_error p ()) @@
    bind_fold_list aux (Environment.full_empty, []) p in
  ok @@ List.rev lst

and type_declaration env : I.declaration -> (environment * O.declaration option) result = function
  | Declaration_type (type_name , type_expression) ->
      let%bind tv = evaluate_type env type_expression in
      let env' = Environment.add_type type_name tv env in
      ok (env', None)
  | Declaration_constant (name , tv_opt , expression) -> (
      let%bind tv'_opt = bind_map_option (evaluate_type env) tv_opt in
      let%bind ae' =
        trace (constant_declaration_error name expression) @@
        type_expression ?tv_opt:tv'_opt env expression in
      let env' = Environment.add_ez_ae name ae' env in
      ok (env', Some (O.Declaration_constant ((make_n_e name ae') , (env , env'))))
    )

and type_match : type i o . (environment -> i -> o result) -> environment -> O.type_value -> i I.matching -> o O.matching result =
  fun f e t i -> match i with
    | Match_bool {match_true ; match_false} ->
      let%bind _ =
        trace_strong (simple_error "Matching bool on not-a-bool")
        @@ get_t_bool t in
      let%bind match_true = f e match_true in
      let%bind match_false = f e match_false in
      ok (O.Match_bool {match_true ; match_false})
  | Match_option {match_none ; match_some} ->
      let%bind t_opt =
        trace_strong (simple_error "Matching option on not-an-option")
        @@ get_t_option t in
      let%bind match_none = f e match_none in
      let (n, b) = match_some in
      let n' = n, t_opt in
      let e' = Environment.add_ez_binder n t_opt e in
      let%bind b' = f e' b in
      ok (O.Match_option {match_none ; match_some = (n', b')})
  | Match_list {match_nil ; match_cons} ->
      let%bind t_list =
        trace_strong (simple_error "Matching list on not-an-list")
        @@ get_t_list t in
      let%bind match_nil = f e match_nil in
      let (hd, tl, b) = match_cons in
      let e' = Environment.add_ez_binder hd t_list e in
      let e' = Environment.add_ez_binder tl t e' in
      let%bind b' = f e' b in
      ok (O.Match_list {match_nil ; match_cons = (hd, tl, b')})
  | Match_tuple (lst, b) ->
      let%bind t_tuple =
        trace_strong (simple_error "Matching tuple on not-a-tuple")
        @@ get_t_tuple t in
      let%bind lst' =
        generic_try (simple_error "Matching tuple of different size")
        @@ (fun () -> List.combine lst t_tuple) in
      let aux prev (name, tv) = Environment.add_ez_binder name tv prev in
      let e' = List.fold_left aux e lst' in
      let%bind b' = f e' b in
      ok (O.Match_tuple (lst, b'))
  | Match_variant lst ->
      let%bind variant_opt =
        let aux acc ((constructor_name , _) , _) =
          let%bind (_ , variant) =
            trace_option (simple_error "bad constructor") @@
            Environment.get_constructor constructor_name e in
          let%bind acc = match acc with
            | None -> ok (Some variant)
            | Some variant' -> (
                Ast_typed.assert_type_value_eq (variant , variant') >>? fun () ->
                ok (Some variant)
              ) in
          ok acc in
        trace (simple_error "in match variant") @@
        bind_fold_list aux None lst in
      let%bind variant =
        trace_option (simple_error "empty variant") @@
        variant_opt in
      let%bind  () =
        let%bind variant_cases' = Ast_typed.Combinators.get_t_sum variant in
        let variant_cases = List.map fst @@ Map.String.to_kv_list variant_cases' in
        let match_cases = List.map (Function.compose fst fst) lst in
        let test_case = fun c ->
          Assert.assert_true (List.mem c match_cases)
        in
        let%bind () =
          trace (simple_error "missing case match") @@
          bind_iter_list test_case variant_cases in
        let%bind () =
          trace_strong (simple_error "redundant case match") @@
          Assert.assert_true List.(length variant_cases = length match_cases) in
        ok ()
      in
      let%bind lst' =
        let aux ((constructor_name , name) , b) =
          let%bind (constructor , _) =
            trace_option (simple_error "bad constructor??") @@
            Environment.get_constructor constructor_name e in
          let e' = Environment.add_ez_binder name constructor e in
          let%bind b' = f e' b in
          ok ((constructor_name , name) , b')
        in
        bind_map_list aux lst in
      ok (O.Match_variant (lst' , variant))

and evaluate_type (e:environment) (t:I.type_expression) : O.type_value result =
  let return tv' = ok (make_t tv' (Some t)) in
  match t with
  | T_function (a, b) ->
      let%bind a' = evaluate_type e a in
      let%bind b' = evaluate_type e b in
      return (T_function (a', b'))
  | T_tuple lst ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (T_tuple lst')
  | T_sum m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (T_sum m)
  | T_record m ->
      let aux k v prev =
        let%bind prev' = prev in
        let%bind v' = evaluate_type e v in
        ok @@ SMap.add k v' prev'
      in
      let%bind m = SMap.fold aux m (ok SMap.empty) in
      return (T_record m)
  | T_variable name ->
      let%bind tv =
        trace_option (unbound_type_variable e name)
        @@ Environment.get_type_opt name e in
      ok tv
  | T_constant (cst, lst) ->
      let%bind lst' = bind_list @@ List.map (evaluate_type e) lst in
      return (T_constant(cst, lst'))

and type_expression : environment -> ?tv_opt:O.type_value -> I.expression -> O.annotated_expression result = fun e ?tv_opt ae ->
  let module L = Logger.Stateful() in
  let return expr tv =
    let%bind () =
      match tv_opt with
      | None -> ok ()
      | Some tv' -> O.assert_type_value_eq (tv' , tv) in
    ok @@ make_a_e expr tv e in
  let main_error =
    let title () = "typing expression" in
    let content () = Format.asprintf "Expression: %a\nLog: %s\n" I.PP.expression ae (L.get()) in
    error title content in
  trace main_error @@
  match ae with
  (* Basic *)
  | E_failwith _ -> simple_fail "can't type failwith in isolation"
  | E_variable name ->
      let%bind tv' =
        trace_option (unbound_variable e name)
        @@ Environment.get_opt name e in
      return (E_variable name) tv'.type_value
  | E_literal (Literal_bool b) ->
      return (E_literal (Literal_bool b)) (t_bool ())
  | E_literal Literal_unit | E_skip ->
      return (E_literal (Literal_unit)) (t_unit ())
  | E_literal (Literal_string s) -> (
      L.log (Format.asprintf "literal_string option type: %a" PP_helpers.(option O.PP.type_value) tv_opt) ;
      match Option.map Ast_typed.get_type' tv_opt with
      | Some (T_constant ("address" , [])) -> return (E_literal (Literal_address s)) (t_address ())
      | _ -> return (E_literal (Literal_string s)) (t_string ())
    )
  | E_literal (Literal_bytes s) ->
      return (E_literal (Literal_bytes s)) (t_bytes ())
  | E_literal (Literal_int n) ->
      return (E_literal (Literal_int n)) (t_int ())
  | E_literal (Literal_nat n) ->
      return (E_literal (Literal_nat n)) (t_nat ())
  | E_literal (Literal_tez n) ->
      return (E_literal (Literal_tez n)) (t_tez ())
  | E_literal (Literal_address s) ->
      return (e_address s) (t_address ())
  | E_literal (Literal_operation op) ->
      return (e_operation op) (t_operation ())
  (* Tuple *)
  | E_tuple lst ->
      let%bind lst' = bind_list @@ List.map (type_expression e) lst in
      let tv_lst = List.map get_type_annotation lst' in
      return (E_tuple lst') (t_tuple tv_lst ())
  | E_accessor (ae, path) ->
      let%bind e' = type_expression e ae in
      let aux (prev:O.annotated_expression) (a:I.access) : O.annotated_expression result =
        match a with
        | Access_tuple index -> (
            let%bind tpl_tv = get_t_tuple prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad tuple index")
              @@ (fun () -> List.nth tpl_tv index) in
            return (E_tuple_accessor (prev , index)) tv
          )
        | Access_record property -> (
            let%bind r_tv = get_t_record prev.type_annotation in
            let%bind tv =
              generic_try (simple_error "bad record index")
              @@ (fun () -> SMap.find property r_tv) in
            return (E_record_accessor (prev , property)) tv
          )
        | Access_map ae -> (
            let%bind ae' = type_expression e ae in
            let%bind (k , v) = get_t_map prev.type_annotation in
            let%bind () =
              Ast_typed.assert_type_value_eq (k , get_type_annotation ae') in
            return (E_look_up (prev , ae')) v
          )
      in
      trace (simple_error "accessing") @@
      bind_fold_list aux e' path

  (* Sum *)
  | E_constructor (c, expr) ->
      let%bind (c_tv, sum_tv) =
        let error =
          let title () = "no such constructor" in
          let content () =
            Format.asprintf "%s in:\n%a\n"
              c O.Environment.PP.full_environment e
          in
          error title content in
        trace_option error @@
        Environment.get_constructor c e in
      let%bind expr' = type_expression e expr in
      let%bind _assert = O.assert_type_value_eq (expr'.type_annotation, c_tv) in
      return (E_constructor (c , expr')) sum_tv
  (* Record *)
  | E_record m ->
      let aux prev k expr =
        let%bind expr' = type_expression e expr in
        ok (SMap.add k expr' prev)
      in
      let%bind m' = bind_fold_smap aux (ok SMap.empty) m in
      return (E_record m') (t_record (SMap.map get_type_annotation m') ())
  (* Data-structure *)
  | E_list lst ->
      let%bind lst' = bind_map_list (type_expression e) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind init = match tv_opt with
          | None -> ok None
          | Some ty ->
              let%bind ty' = get_t_list ty in
              ok (Some ty') in
        let%bind ty =
          let%bind opt = bind_fold_list aux init
          @@ List.map get_type_annotation lst' in
          trace_option (simple_error "empty list expression without annotation") opt in
        ok (t_list ty ())
      in
      return (E_list lst') tv
  | E_map lst ->
      let%bind lst' = bind_map_list (bind_map_pair (type_expression e)) lst in
      let%bind tv =
        let aux opt c =
          match opt with
          | None -> ok (Some c)
          | Some c' ->
              let%bind _eq = Ast_typed.assert_type_value_eq (c, c') in
              ok (Some c') in
        let%bind key_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_annotation
            @@ List.map fst lst' in
          let%bind annot = bind_map_option get_t_map_key tv_opt in
          trace (simple_error "untyped empty map expression") @@
          O.merge_annotation annot sub
        in
        let%bind value_type =
          let%bind sub =
            bind_fold_list aux None
            @@ List.map get_type_annotation
            @@ List.map snd lst' in
          let%bind annot = bind_map_option get_t_map_value tv_opt in
          trace (simple_error "untyped empty map expression") @@
          O.merge_annotation annot sub
        in
        ok (t_map key_type value_type ())
      in
      return (E_map lst') tv
  | E_lambda {
      binder ;
      input_type ;
      output_type ;
      result ;
    } -> (
      let%bind input_type =
        let%bind input_type =
          trace_option (simple_error "no input type provided") @@
          input_type in
        evaluate_type e input_type in
      let%bind output_type =
        let%bind output_type =
          trace_option (simple_error "no output type provided") @@
          output_type in
        evaluate_type e output_type in
      let e' = Environment.add_ez_binder (fst binder) input_type e in
      let%bind result = type_expression ~tv_opt:output_type e' result in
      return (E_lambda {binder = fst binder;input_type;output_type;result}) (t_function input_type output_type ())
    )
  | E_constant (name, lst) ->
      let%bind lst' = bind_list @@ List.map (type_expression e) lst in
      let tv_lst = List.map get_type_annotation lst' in
      let%bind (name', tv) = type_constant name tv_lst tv_opt in
      return (E_constant (name' , lst')) tv
  | E_application (f, arg) ->
      let%bind f = type_expression e f in
      let%bind arg = type_expression e arg in
      let%bind tv = match f.type_annotation.type_value' with
        | T_function (param, result) ->
            let%bind _ = O.assert_type_value_eq (param, arg.type_annotation) in
            ok result
        | _ -> simple_fail "applying to not-a-function"
      in
      return (E_application (f , arg)) tv
  | E_look_up dsi ->
      let%bind (ds, ind) = bind_map_pair (type_expression e) dsi in
      let%bind (src, dst) = get_t_map ds.type_annotation in
      let%bind _ = O.assert_type_value_eq (ind.type_annotation, src) in
      return (E_look_up (ds , ind)) (t_option dst ())
  (* Advanced *)
  | E_matching (ex, m) -> (
      let%bind ex' = type_expression e ex in
      match m with
      (* Special case for assert-like failwiths. TODO: CLEAN THIS. *)
      | I.Match_bool { match_false ; match_true = E_failwith fw } -> (
          let%bind fw' = type_expression e fw in
          let%bind mf' = type_expression e match_false in
          let%bind () =
            trace_strong (simple_error "Matching bool on not-a-bool")
            @@ assert_t_bool (get_type_annotation ex') in
          let%bind () =
            trace_strong (simple_error "Matching not-unit on an assert")
            @@ assert_t_unit (get_type_annotation mf') in
          let mt' = make_a_e
              (E_constant ("ASSERT" , [ex' ; fw']))
              (t_unit ())
              e
          in
          let m' = O.Match_bool { match_true = mt' ; match_false = mf' } in
          return (O.E_matching (ex' , m')) (t_unit ())
        )
      | _ -> (
          let%bind m' = type_match (type_expression ?tv_opt:None) e ex'.type_annotation m in
          let tvs =
            let aux (cur:O.value O.matching) =
              match cur with
              | Match_bool { match_true ; match_false } -> [ match_true ; match_false ]
              | Match_list { match_nil ; match_cons = (_ , _ , match_cons) } -> [ match_nil ; match_cons ]
              | Match_option { match_none ; match_some = (_ , match_some) } -> [ match_none ; match_some ]
              | Match_tuple (_ , match_tuple) -> [ match_tuple ]
              | Match_variant (lst , _) -> List.map snd lst in
            List.map get_type_annotation @@ aux m' in
          let aux prec cur =
            let%bind () =
              match prec with
              | None -> ok ()
              | Some cur' -> Ast_typed.assert_type_value_eq (cur , cur') in
            ok (Some cur) in
          let%bind tv_opt = bind_fold_list aux None tvs in
          let%bind tv =
            trace_option (simple_error "empty matching") @@
            tv_opt in
          return (O.E_matching (ex', m')) tv
        )
    )
  | E_sequence (a , b) ->
    let%bind a' = type_expression e a in
    let%bind b' = type_expression e b in
    let%bind () =
      trace_strong (simple_error "first part of the sequence isn't of unit type") @@
      Ast_typed.assert_type_value_eq (t_unit () , get_type_annotation a') in
    return (O.E_sequence (a' , b')) (get_type_annotation b')
  | E_loop (expr , body) ->
    let%bind expr' = type_expression e expr in
    let%bind body' = type_expression e body in
    let%bind () =
      trace_strong (simple_error "while condition isn't of type bool") @@
      Ast_typed.assert_type_value_eq (t_bool () , get_type_annotation expr') in
    let%bind () =
      trace_strong (simple_error "while body isn't of unit type") @@
      Ast_typed.assert_type_value_eq (t_unit () , get_type_annotation body') in
    return (O.E_loop (expr' , body')) (t_unit ())
  | E_assign (name , path , expr) ->
    let%bind typed_name =
      let%bind ele = Environment.get_trace name e in
      ok @@ make_n_t name ele.type_value in
    let%bind (assign_tv , path') =
      let aux : ((_ * O.access_path) as 'a) -> I.access -> 'a result = fun (prec_tv , prec_path) cur_path ->
        match cur_path with
        | Access_tuple index -> (
            let%bind tpl = get_t_tuple prec_tv in
            let%bind tv' =
              trace_option (simple_error "tuple too small") @@
              List.nth_opt tpl index in
            ok (tv' , prec_path @ [O.Access_tuple index])
          )
        | Access_record property -> (
            let%bind m = get_t_record prec_tv in
            let%bind tv' =
              trace_option (simple_error "tuple too small") @@
              Map.String.find_opt property m in
            ok (tv' , prec_path @ [O.Access_record property])
          )
        | Access_map _ -> simple_fail "no assign expressions with maps yet"
      in
      bind_fold_list aux (typed_name.type_value , []) path in
    let%bind expr' = type_expression e expr in
    let%bind () =
      trace_strong (simple_error "assign type doesn't match left-hand-side") @@
      Ast_typed.assert_type_value_eq (assign_tv , get_type_annotation expr') in
    return (O.E_assign (typed_name , path' , expr')) (t_unit ())
  | E_let_in {binder ; rhs ; result} ->
    let%bind rhs_tv_opt = bind_map_option (evaluate_type e) (snd binder) in
    let%bind rhs = type_expression ?tv_opt:rhs_tv_opt e rhs in
    let e' = Environment.add_ez_declaration (fst binder) rhs e in
    let%bind result = type_expression e' result in
    return (E_let_in {binder = fst binder; rhs; result}) result.type_annotation
  | E_annotation (expr , te) ->
    let%bind tv = evaluate_type e te in
    let%bind expr' = type_expression ~tv_opt:tv e expr in
    let%bind type_annotation = O.merge_annotation (Some tv) (Some expr'.type_annotation) in
    ok {expr' with type_annotation}


and type_constant (name:string) (lst:O.type_value list) (tv_opt:O.type_value option) : (string * O.type_value) result =
  (* Constant poorman's polymorphism *)
  let ct = Operators.Typer.constant_typers in
  let%bind typer =
    trace_option (unrecognized_constant name) @@
    Map.String.find_opt name ct in
  typer lst tv_opt

let untype_type_value (t:O.type_value) : (I.type_expression) result =
  match t.simplified with
  | Some s -> ok s
  | _ -> simple_fail "trying to untype generated type"

let untype_literal (l:O.literal) : I.literal result =
  let open I in
  match l with
  | Literal_unit -> ok Literal_unit
  | Literal_bool b -> ok (Literal_bool b)
  | Literal_nat n -> ok (Literal_nat n)
  | Literal_tez n -> ok (Literal_tez n)
  | Literal_int n -> ok (Literal_int n)
  | Literal_string s -> ok (Literal_string s)
  | Literal_bytes b -> ok (Literal_bytes b)
  | Literal_address s -> ok (Literal_address s)
  | Literal_operation s -> ok (Literal_operation s)

let rec untype_expression (e:O.annotated_expression) : (I.expression) result =
  let open I in
  let return e = ok e in
  match e.expression with
  | E_literal l ->
      let%bind l = untype_literal l in
      return (E_literal l)
  | E_constant (n, lst) ->
      let%bind lst' = bind_list
        @@ List.map untype_expression lst in
      return (E_constant (n, lst'))
  | E_variable n ->
      return (E_variable n)
  | E_application (f, arg) ->
      let%bind f' = untype_expression f in
      let%bind arg' = untype_expression arg in
      return (E_application (f', arg'))
  | E_lambda {binder;input_type;output_type;result} ->
      let%bind input_type = untype_type_value input_type in
      let%bind output_type = untype_type_value output_type in
      let%bind result = untype_expression result in
      return (E_lambda {binder = (binder , Some input_type);input_type = Some input_type;output_type = Some output_type;result})
  | E_tuple lst ->
      let%bind lst' = bind_list
        @@ List.map untype_expression lst in
      return (E_tuple lst')
  | E_tuple_accessor (tpl, ind)  ->
      let%bind tpl' = untype_expression tpl in
      return (E_accessor (tpl', [Access_tuple ind]))
  | E_constructor (n, p) ->
      let%bind p' = untype_expression p in
      return (E_constructor (n, p'))
  | E_record r ->
      let%bind r' = bind_smap
        @@ SMap.map untype_expression r in
      return (E_record r')
  | E_record_accessor (r, s) ->
      let%bind r' = untype_expression r in
      return (E_accessor (r', [Access_record s]))
  | E_map m ->
      let%bind m' = bind_map_list (bind_map_pair untype_expression) m in
      return (E_map m')
  | E_list lst ->
      let%bind lst' = bind_map_list untype_expression lst in
      return (E_list lst')
  | E_look_up dsi ->
      let%bind dsi' = bind_map_pair untype_expression dsi in
      return (E_look_up dsi')
  | E_matching (ae, m) ->
      let%bind ae' = untype_expression ae in
      let%bind m' = untype_matching untype_expression m in
      return (E_matching (ae', m'))
  | E_failwith ae ->
      let%bind ae' = untype_expression ae in
      return (E_failwith ae')
  | E_sequence _
  | E_loop _
  | E_assign _ -> simple_fail "not possible to untranspile statements yet"
  | E_let_in {binder;rhs;result} ->
      let%bind tv = untype_type_value rhs.type_annotation in
      let%bind rhs = untype_expression rhs in
      let%bind result = untype_expression result in
      return (E_let_in {binder = (binder , Some tv);rhs;result})

and untype_matching : type o i . (o -> i result) -> o O.matching -> (i I.matching) result = fun f m ->
  let open I in
  match m with
  | Match_bool {match_true ; match_false} ->
      let%bind match_true = f match_true in
      let%bind match_false = f match_false in
      ok @@ Match_bool {match_true ; match_false}
  | Match_tuple (lst, b) ->
      let%bind b = f b in
      ok @@ Match_tuple (lst, b)
  | Match_option {match_none ; match_some = (v, some)} ->
      let%bind match_none = f match_none in
      let%bind some = f some in
      let match_some = fst v, some in
      ok @@ Match_option {match_none ; match_some}
  | Match_list {match_nil ; match_cons = (hd, tl, cons)} ->
      let%bind match_nil = f match_nil in
      let%bind cons = f cons in
      let match_cons = hd, tl, cons in
      ok @@ Match_list {match_nil ; match_cons}
  | Match_variant (lst , _) ->
      let aux ((a,b),c) =
        let%bind c' = f c in
        ok ((a,b),c') in
      let%bind lst' = bind_map_list aux lst in
      ok @@ Match_variant lst'
