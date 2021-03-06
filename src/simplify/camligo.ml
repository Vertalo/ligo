open Trace
open Function
module I = Parser.Camligo.Ast
module O = Ast_simplified
open O.Combinators

let unwrap : type a . a Location.wrap -> a = Location.unwrap

open Operators.Simplify.Camligo

let type_variable : string -> O.type_expression result = fun str ->
  match List.assoc_opt str type_constants with
  | Some s -> ok @@ O.T_constant (s , [])
  | None -> ok @@ O.T_variable str

let get_param_restricted_pattern : I.param -> I.restricted_pattern Location.wrap result = fun p ->
  match p with
  | I.Param_restricted_pattern c -> ok c
  | _ ->
      let error =
        let title () = "not a restricted param pattern" in
        let content () = Format.asprintf "%a" I.pp_param p in
        error title content in
      fail error

let get_unrestricted_pattern : I.restricted_pattern -> I.pattern Location.wrap result = fun rp ->
  match rp with
  | I.Pr_restrict p -> ok p
  | _ ->
      let error =
        let title () = "not an unrestricted param pattern" in
        let content () = Format.asprintf "%a" I.pp_restricted_pattern rp in
        error title content in
      fail error

let get_p_type_annotation : I.pattern -> (I.pattern Location.wrap * I.restricted_type_expression Location.wrap) result = fun p ->
  match p with
  | I.P_type_annotation pta -> ok pta
  | _ -> simple_fail "not a pattern type annotation"

let get_p_variable : I.pattern -> string Location.wrap result = fun p ->
  match p with
  | I.P_variable v -> ok v
  | _ -> simple_fail "not a pattern variable"

let get_p_option_typed_variable : I.pattern -> (string Location.wrap * I.restricted_type_expression Location.wrap option) result = fun p ->
  match p with
  | I.P_variable v -> ok (v , None)
  | I.P_type_annotation (pat , rte) -> (
      let%bind v = get_p_variable @@ unwrap pat in
      ok (v , Some rte)
    )
  | _ -> simple_fail "not an optionally typed pattern variable"

let get_p_typed_variable : I.pattern -> (string Location.wrap * I.restricted_type_expression Location.wrap) result = fun p ->
  let%bind (p' , rte) =
    trace (simple_error "get_p_typed_variable") @@
    get_p_type_annotation p in
  let%bind var = get_p_variable (unwrap p') in
  ok (var , rte)

let get_eh_accessor : _ -> _ result = fun x ->
  match x with
  | I.Eh_accessor x -> ok x
  | _ -> simple_fail "not a simple eh_accessor"

let get_typed_variable_param : I.param -> _ result = fun arg ->
  let%bind up =
    let%bind rp = get_param_restricted_pattern arg in
    let%bind up = get_unrestricted_pattern (unwrap rp) in
    ok up in
  let%bind (var , rte) = get_p_typed_variable (unwrap up) in
  ok (var , rte)

let get_untyped_variable_param : I.param -> _ result = fun arg ->
  let%bind rp = get_param_restricted_pattern arg in
  let%bind var = match (unwrap rp) with
    | I.Pr_variable v -> ok v
    | _ -> simple_fail "a regular variable was expected" in
  ok var

let get_type_annotation_ : I.type_annotation_ -> I.type_expression Location.wrap result = fun p ->
    match p with
    | I.Type_annotation_ p -> ok p

let get_e_match_clause : I.e_match_clause -> (I.pattern Location.wrap * I.expression_no_match Location.wrap) result = fun e ->
  match e with
  | E_match_clause c -> ok c

let match_clauses : type a . (I.pattern * a) list -> a O.matching result = fun _clauses ->
  let match_bool _ = simple_fail "" in
  let match_stuff _ = simple_fail "" in
  bind_find_map_list (simple_error "no weird matching yet") (fun f -> f ()) [ match_bool ; match_stuff ]

let rec of_no_match : I.expression_no_match -> I.expression = fun enm ->
  let open I in
  let self = Location.map of_no_match in
  match enm with
  | Em_let_in (a, b, c) -> E_let_in (a , self b , self c)
  | Em_fun (a , b) -> E_fun (a , self b)
  | Em_record r -> E_record r
  | Em_ifthenelse (a , b , c) -> E_ifthenelse (self a , self b , self c)
  | Em_ifthen (a , b) -> E_ifthen (self a , self b)
  | Em_main m -> E_main m

let rec of_no_seq : I.expression_no_seq -> I.expression = fun enm ->
  let open I in
  let self = Location.map of_no_seq in
  match enm with
  | Es_let_in (a, b, c) -> E_let_in (a , self b , self c)
  | Es_fun (a , b) -> E_fun (a , self b)
  | Es_record r -> E_record r
  | Es_ifthenelse (a , b , c) -> E_ifthenelse (self a , self b , self c)
  | Es_ifthen (a , b) -> E_ifthen (self a , self b)
  | Es_match (a , b) -> E_match (self a , b)
  | Es_main m -> E_main m

let rec type_expression : I.type_expression -> O.type_expression result = fun te ->
  match te with
  | T_variable tv ->
      let%bind tv' = bind_map_location type_variable tv in
      ok @@ unwrap tv'
  | T_tuple lst ->
      let%bind lst' = bind_map_list (bind_map_location type_expression) lst in
      ok @@ O.T_tuple (List.map unwrap lst')
  | T_paren p ->
      let%bind p' = bind_map_location type_expression p in
      ok @@ unwrap p'
  | T_record r ->
      let aux : I.t_record_element -> _ = fun (T_record_element (s, te)) ->
        let%bind te' = bind_map_location type_expression te in
        ok (s, te')
      in
      let%bind r' = bind_map_list (bind_map_location aux) r in
      let te_map =
        let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
        let open Map.String in
        List.fold_left (fun prec (k , v) -> add k v prec) empty lst
      in
      ok @@ O.T_record te_map
  | T_application (arg , f) ->
      let%bind arg' = bind_map_location type_expression arg in
      match unwrap f with
        | I.T_variable v -> (
            match List.assoc_opt v.wrap_content type_constants with
            | Some s -> (
                match arg'.wrap_content with
                | T_tuple lst -> ok @@ O.T_constant (s , lst)
                | e -> ok @@ O.T_constant (s , [ e ])
              )
            | None -> (
                let error =
                  let title () = "unrecognized type-constant" in
                  let content () = Format.asprintf "%s" v.wrap_content in
                  error title content
                in
                fail error
              )
          )
        | _ -> simple_fail "type applying to non-var"

let rec of_restricted_type_expression : I.restricted_type_expression -> I.type_expression = fun rte ->
  let self = of_restricted_type_expression in
  match rte with
  | Tr_variable tv -> T_variable tv
  | Tr_application (a , b) -> T_application (Location.map self a , Location.map self b)
  | Tr_paren te -> unwrap te

let restricted_type_expression : I.restricted_type_expression -> O.type_expression result =
  Function.compose type_expression of_restricted_type_expression

let rec expression : I.expression -> O.expression result = fun e ->
  match e with
  | I.E_sequence lst -> (
      let%bind lst' = bind_map_list expression @@ List.map unwrap lst in
      match lst' with
      | [] -> simple_fail "empty sequence"
      | hd :: tl -> ok @@ List.fold_right' (fun prec cur -> e_sequence prec cur) hd tl
    )
  | I.E_let_in (pattern , expr , body) -> (
      let%bind (name , rte) = get_p_option_typed_variable @@ unwrap pattern in
      let%bind type_expression' = bind_map_option (fun x -> restricted_type_expression @@ unwrap x) rte in
      let%bind expr' = expression @@ unwrap expr in
      let%bind body' = expression @@ unwrap body in
      ok @@ e_let_in (unwrap name , type_expression') expr' body'
    )
  | I.E_ifthenelse ite -> ifthenelse ite
  | I.E_ifthen it -> ifthen it
  | I.E_match m -> match_ m
  | I.E_record r -> record r
  | I.E_fun (pattern , expr) -> (
      let%bind (name , rte) = get_p_typed_variable @@ unwrap pattern in
      let name' = unwrap name in
      let%bind type_expression' = restricted_type_expression (unwrap rte) in
      let%bind expr' = expression (unwrap expr) in
      ok @@ e_lambda name'
        (Some type_expression') None
        expr'
    )
  | I.E_main m -> expression_main m

and ifthenelse
  : (I.expression Location.wrap * I.expression Location.wrap * I.expression Location.wrap) -> O.expression result
  = fun ite ->
  let (cond , branch_true , branch_false) = ite in
  let%bind cond' = bind_map_location expression cond in
  let%bind branch_true' = bind_map_location expression branch_true in
  let%bind branch_false' = bind_map_location expression branch_false in
  ok @@ O.(e_match_bool (unwrap cond') (unwrap branch_true') (unwrap branch_false'))

and ifthen
  : (I.expression Location.wrap * I.expression Location.wrap) -> O.expression result
  = fun it ->
  let (cond , branch_true) = it in
  let%bind cond' = bind_map_location expression cond in
  let%bind branch_true' = bind_map_location expression branch_true in
  ok @@ O.(e_match_bool (unwrap cond') (unwrap branch_true') (e_unit ()))

and match_
  : I.expression Location.wrap * I.e_match_clause Location.wrap list -> O.expression result
  = fun m ->
    let (expr , clauses) = m in
    let%bind expr' = expression (unwrap expr) in
    let%bind clauses' =
      let%bind clauses =
        bind_map_list get_e_match_clause
        @@ List.map unwrap clauses in
      let aux (x , y) =
        let x' = unwrap x in
        let%bind y' = expression @@ of_no_match @@ unwrap y in
        ok (x' , y') in
      bind_map_list aux clauses in
    let%bind matching = match_clauses clauses' in
    ok O.(e_match expr' matching)

and record
  = fun r ->
  let aux : I.e_record_element -> _ = fun re ->
    match re with
    | E_record_element_record_implicit _ -> simple_fail "no implicit record element yet"
    | E_record_element_record_explicit (s, e) ->
        let%bind e' = bind_map_location (Function.compose expression of_no_seq) e in
        ok (s, e')
  in
  let%bind r' = bind_map_list (bind_map_location aux) r in
  let lst = List.map ((fun (x, y) -> unwrap x, unwrap y) >| unwrap) r' in
  ok @@ O.(e_record lst)

and expression_main : I.expression_main Location.wrap -> O.expression result = fun em ->
  let return x = ok @@ x in
  let simple_binop name ab =
    let%bind (a' , b') = bind_map_pair expression_main ab in
    return @@ e_binop name a' b' in
  let error_main =
    let title () = "simplifying main_expression" in
    let content () = Format.asprintf "%a" I.pp_expression_main (unwrap em) in
    error title content
  in
  trace error_main @@
  match (unwrap em) with
  | Eh_tuple lst ->
      let%bind lst' = bind_map_list expression_main lst in
      return @@ e_tuple lst'
  | Eh_module_ident (lst , v) -> identifier_application (lst , v) None
  | Eh_variable v -> identifier_application ([] , v) None
  | Eh_application (f , arg) -> (
      let%bind arg' = expression_main arg in
      match unwrap f with
      | Eh_variable v -> identifier_application ([] , v) (Some arg')
      | Eh_module_ident (lst , v) -> identifier_application (lst , v) (Some arg')
      | _ -> (
          let%bind f' = expression_main f in
          return @@ e_application f' arg'
        )
    )
  | Eh_type_annotation (e, te) -> (
      let%bind e' = expression_main e in
      let%bind te' = bind_map_location restricted_type_expression te in
      ok @@ e_annotation e' (unwrap te')
    )
  | Eh_lt ab ->
      simple_binop "LT" ab
  | Eh_gt ab ->
      simple_binop "GT" ab
  | Eh_le ab ->
      simple_binop "LE" ab
  | Eh_eq ab ->
      simple_binop "EQ" ab
  | Eh_neq ab ->
      simple_binop "NEQ" ab
  | Eh_cons ab ->
      simple_binop "CONS" ab
  | Eh_addition ab ->
      simple_binop "ADD" ab
  | Eh_substraction ab ->
      simple_binop "MINUS" ab
  | Eh_multiplication ab ->
      simple_binop "TIMES" ab
  | Eh_division ab ->
      simple_binop "DIV" ab
  | Eh_int n ->
      return @@ e_int (unwrap n)
  | Eh_string s ->
      return @@ e_string (unwrap s)
  | Eh_unit _ ->
      return @@ e_unit ()
  | Eh_tz n ->
      return @@ e_tez (unwrap n)
  | Eh_constructor _ ->
      simple_fail "constructor without parameter"
  | Eh_data_structure (kind , content) -> (
      match unwrap kind with
      | "list" -> (
          let%bind lst = bind_map_list expression_main content in
          ok @@ e_list lst
        )
      | kind' -> (
          let error =
            let title () = "data-structures not supported yet" in
            let content () = Format.asprintf "%s" kind' in
            error title content in
          fail error
        )
    )
  | Eh_name _ ->
      simple_fail "named parameter not supported yet"
  | Eh_assign x ->
      simple_binop "ASSIGN" x
  | Eh_accessor (src , path) ->
      ok @@ O.(e_accessor_props (e_variable (unwrap src)) (List.map unwrap path))
  | Eh_bottom e ->
      expression (unwrap e)

and identifier_application : (string Location.wrap) list * string Location.wrap -> O.expression option -> _ result = fun (lst , v) param_opt ->
  let constant_name = String.concat "." ((List.map unwrap lst) @ [unwrap v]) in
  match List.assoc_opt constant_name constants , param_opt with
  | Some s , None -> ok O.(E_constant (s , []))
  | Some s , Some param -> (
      let params =
        match param with
        | E_tuple lst -> lst
        | _ -> [ param ] in
      ok O.(E_constant (s , params))
    )
  | None , param_opt -> (
      let%bind () =
        let error =
          let title () = "no module identifiers yet" in
          let content () = Format.asprintf "%s" constant_name in
          error title content in
        trace_strong error @@
        Assert.assert_list_empty lst in
      match (constant_name , param_opt) with
      | "failwith" , Some param -> ok O.(e_failwith param)
      | _ , Some param -> ok @@ e_application (e_variable (unwrap v)) param
      | _ , None -> ok @@ e_variable (unwrap v)
    )

let let_content : I.let_content -> _ result = fun l ->
  match l with
  | (Let_content (n, args, ty_opt, e)) -> (
      let%bind args' = bind_map_list (bind_map_location get_typed_variable_param) args in
      let%bind ty' =
        let%bind tya =
          trace_option (simple_error "top-level declarations need a type") @@
          ty_opt in
        let%bind ty = get_type_annotation_ (unwrap tya) in
        bind_map_location type_expression ty in
      match args' with
      | [] -> ( (* No arguments. Simplify as regular value. *)
          let%bind e' = bind_map_location expression e in
          ok @@ O.Declaration_constant (unwrap n , Some (unwrap ty') , unwrap e')
        )
      | [_param] ->
          simple_fail "no syntactic sugar for functions yet param"
      | _lst -> ( (* Arguments without fun. *)
          simple_fail "if you want currified functions, please do so explicitly"
        )
    )

let let_entry : _ -> _ result = fun l ->
  let (I.Let_content (n , args , ty_opt , e)) = l in
  let%bind () =
    trace_strong (simple_error "entry-point shouldn't have type annotations") @@
    Assert.assert_none ty_opt in
  let%bind (param , storage) =
    trace_option (simple_error "entry-points should have exactly two params") @@
    List.to_pair args in
  let%bind (param_name , param_ty) =
    let%bind param' = bind_map_location get_typed_variable_param param in
    let (param_name , param_ty) = unwrap param' in
    let param_name' = unwrap param_name in
    let%bind param_ty' = restricted_type_expression (unwrap param_ty) in
    ok (param_name' , param_ty') in
  let%bind storage_name = get_untyped_variable_param (unwrap storage) in
  let storage_ty = O.T_variable "storage" in
  let (arguments_name , input_ty) =
    let ty = t_tuple [param_ty ; storage_ty] in
    let nty = ("arguments" , ty) in
    nty in
  let tpl_declarations =
    let aux = fun i (name , type_expression) expr ->
      e_let_in
        (name , Some type_expression)
        (e_accessor (e_variable arguments_name) [ Access_tuple i ])
        expr
    in
    let lst = List.mapi aux [ (param_name , param_ty) ; ((unwrap storage_name) , storage_ty)] in
    fun expr -> List.fold_right' (fun prec cur -> cur prec) expr lst
  in
  let%bind result = expression (unwrap e) in
  let result = tpl_declarations result in
  let input_type' = input_ty in
  let output_type' = O.(t_pair (t_list t_operation , storage_ty)) in
  let lambda = e_lambda
    arguments_name
    (Some input_ty) (Some output_type')
    result in
  let type_annotation = Some (O.T_function (input_type', output_type')) in
  ok @@ O.Declaration_constant (unwrap n , type_annotation , lambda)

let let_init_storage : _ -> _ result = fun l ->
  let (args , ty_opt , e) = l in
  let%bind () =
    trace_strong (simple_error "storage init shouldn't have a type annotation") @@
    Assert.assert_none ty_opt in
  let%bind () =
    trace (simple_error "storage init should have no parameter (address)") @@
    Assert.assert_list_size args 0 in
  let%bind content =
    let%bind expr = bind_map_location expression e in
    ok expr
  in
  let type_annotation = O.t_variable "storage" in
  ok @@ O.(Declaration_constant ("storage" , Some type_annotation , (unwrap content)))


let let_init_content : I.let_content -> _ result = fun l ->
  let (I.Let_content (n, args, ty_opt, e)) = l in
  match (unwrap n) with
  | "storage" -> let_init_storage (args , ty_opt , e)
  | _ -> simple_fail "%init directives are only used for storage"

let statement : I.statement -> O.declaration result = fun s ->
  match s with
  | Statement_variable_declaration x -> let_content (unwrap x)
  | Statement_init_declaration x -> let_init_content (unwrap x)
  | Statement_entry_declaration x -> let_entry (unwrap x)
  | Statement_type_declaration (n, te) ->
      let%bind te' = bind_map_location type_expression te in
      ok @@ O.Declaration_type (unwrap n , unwrap te')

let program : I.program -> O.program result = fun (Program lst) ->
  bind_map_list (bind_map_location statement) lst

let main : I.entry_point -> O.program Location.wrap result =
  bind_map_location program
