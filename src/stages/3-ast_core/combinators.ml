open Types
open Simple_utils.Trace
module Option = Simple_utils.Option
module SMap = Map.String

module Errors = struct
  let bad_kind expected location =
    let title () = Format.asprintf "a %s was expected" expected in
    let message () = "" in
    let data =
      [("location", fun () -> Format.asprintf "%a" Location.pp location)]
    in
    error ~data title message

  let bad_type_operator type_op =
    let title () =
      Format.asprintf
        "bad type operator %a"
        (PP.type_operator PP.type_expression)
        type_op
    in
    let message () = "" in
    error title message
end

open Errors

let make_t type_content = {type_content; type_meta = ()}

let tuple_to_record lst =
  let aux (i, acc) el = (i + 1, (string_of_int i, el) :: acc) in
  let (_, lst) = List.fold_left aux (0, []) lst in
  lst

let t_bool : type_expression = make_t @@ T_constant TC_bool

let t_string : type_expression = make_t @@ T_constant TC_string

let t_bytes : type_expression = make_t @@ T_constant TC_bytes

let t_int : type_expression = make_t @@ T_constant TC_int

let t_operation : type_expression = make_t @@ T_constant TC_operation

let t_nat : type_expression = make_t @@ T_constant TC_nat

let t_tez : type_expression = make_t @@ T_constant TC_mutez

let t_unit : type_expression = make_t @@ T_constant TC_unit

let t_address : type_expression = make_t @@ T_constant TC_address

let t_signature : type_expression = make_t @@ T_constant TC_signature

let t_key : type_expression = make_t @@ T_constant TC_key

let t_key_hash : type_expression = make_t @@ T_constant TC_key_hash

let t_timestamp : type_expression = make_t @@ T_constant TC_timestamp

let t_option o : type_expression = make_t @@ T_operator (TC_option o)

let t_list t : type_expression = make_t @@ T_operator (TC_list t)

let t_variable n : type_expression = make_t @@ T_variable (Var.of_name n)

let t_record_ez lst =
  let lst = List.map (fun (k, v) -> (Label k, v)) lst in
  let m = LMap.of_list lst in
  make_t @@ T_record m

let t_record m : type_expression =
  let lst = Map.String.to_kv_list m in
  t_record_ez lst

let t_pair (a, b) : type_expression = t_record_ez [("0", a); ("1", b)]

let t_tuple lst : type_expression = t_record_ez (tuple_to_record lst)

let ez_t_sum (lst : (string * type_expression) list) : type_expression =
  let aux prev (k, v) = CMap.add (Constructor k) v prev in
  let map = List.fold_left aux CMap.empty lst in
  make_t @@ T_sum map

let t_sum m : type_expression =
  let lst = Map.String.to_kv_list m in
  ez_t_sum lst

let t_function type1 type2 : type_expression = make_t @@ T_arrow {type1; type2}

let t_map key value : type_expression =
  make_t @@ T_operator (TC_map (key, value))

let t_big_map key value : type_expression =
  make_t @@ T_operator (TC_big_map (key, value))

let t_set key : type_expression = make_t @@ T_operator (TC_set key)

let t_contract contract : type_expression =
  make_t @@ T_operator (TC_contract contract)

(* TODO find a better way than using list*)
let t_operator op lst : type_expression result =
  match (op, lst) with
  | (TC_set _, [t]) ->
      ok @@ t_set t
  | (TC_list _, [t]) ->
      ok @@ t_list t
  | (TC_option _, [t]) ->
      ok @@ t_option t
  | (TC_map (_, _), [kt; vt]) ->
      ok @@ t_map kt vt
  | (TC_big_map (_, _), [kt; vt]) ->
      ok @@ t_big_map kt vt
  | (TC_contract _, [t]) ->
      ok @@ t_contract t
  | (_, _) ->
      fail @@ bad_type_operator op

let make_expr ?(loc = Location.generated) expression_content =
  let location = loc in
  {expression_content; location}

let e_var ?loc (n : string) : expression =
  make_expr ?loc @@ E_variable (Var.of_name n)

let e_literal ?loc l : expression = make_expr ?loc @@ E_literal l

let e_unit ?loc () : expression = make_expr ?loc @@ E_literal Literal_unit

let e_int ?loc n : expression = make_expr ?loc @@ E_literal (Literal_int n)

let e_nat ?loc n : expression = make_expr ?loc @@ E_literal (Literal_nat n)

let e_timestamp ?loc n : expression =
  make_expr ?loc @@ E_literal (Literal_timestamp n)

let e_bool ?loc b : expression = make_expr ?loc @@ E_literal (Literal_bool b)

let e_string ?loc s : expression =
  make_expr ?loc @@ E_literal (Literal_string s)

let e_address ?loc s : expression =
  make_expr ?loc @@ E_literal (Literal_address s)

let e_mutez ?loc s : expression = make_expr ?loc @@ E_literal (Literal_mutez s)

let e_signature ?loc s : expression =
  make_expr ?loc @@ E_literal (Literal_signature s)

let e_key ?loc s : expression = make_expr ?loc @@ E_literal (Literal_key s)

let e_key_hash ?loc s : expression =
  make_expr ?loc @@ E_literal (Literal_key_hash s)

let e_chain_id ?loc s : expression =
  make_expr ?loc @@ E_literal (Literal_chain_id s)

let e'_bytes b : expression_content result =
  let%bind bytes =
    generic_try (simple_error "bad hex to bytes") (fun () ->
        Hex.to_bytes (`Hex b))
  in
  ok @@ E_literal (Literal_bytes bytes)

let e_bytes_hex ?loc b : expression result =
  let%bind e' = e'_bytes b in
  ok @@ make_expr ?loc e'

let e_bytes_raw ?loc (b : bytes) : expression =
  make_expr ?loc @@ E_literal (Literal_bytes b)

let e_bytes_string ?loc (s : string) : expression =
  make_expr ?loc @@ E_literal (Literal_bytes (Hex.to_bytes (Hex.of_string s)))

let e_some ?loc s : expression =
  make_expr ?loc @@ E_constant {cons_name = C_SOME; arguments = [s]}

let e_none ?loc () : expression =
  make_expr ?loc @@ E_constant {cons_name = C_NONE; arguments = []}

let e_string_cat ?loc sl sr : expression =
  make_expr ?loc @@ E_constant {cons_name = C_CONCAT; arguments = [sl; sr]}

let e_map_add ?loc k v old : expression =
  make_expr ?loc @@ E_constant {cons_name = C_MAP_ADD; arguments = [k; v; old]}

let e_constructor ?loc s a : expression =
  make_expr ?loc @@ E_constructor {constructor = Constructor s; element = a}

let e_matching ?loc a b : expression =
  make_expr ?loc @@ E_matching {matchee = a; cases = b}

let e_matching_bool ?loc a b c : expression =
  e_matching ?loc a (Match_bool {match_true = b; match_false = c})

let e_record_accessor ?loc a b =
  make_expr ?loc @@ E_record_accessor {record = a; path = Label b}

let e_record_accessor_list ?loc a b =
  List.fold_left (fun a b -> e_record_accessor ?loc a b) a b

let e_variable ?loc v = make_expr ?loc @@ E_variable v

let e_let_in ?loc (binder, ascr) inline rhs let_result =
  make_expr ?loc
  @@ E_let_in {let_binder = (binder, ascr); rhs; let_result; inline}

let e_annotation ?loc anno_expr ty =
  make_expr ?loc @@ E_ascription {anno_expr; type_annotation = ty}

let e_application ?loc a b =
  make_expr ?loc @@ E_application {lamb = a; args = b}

let e_binop ?loc name a b =
  make_expr ?loc @@ E_constant {cons_name = name; arguments = [a; b]}

let e_constant ?loc name lst =
  make_expr ?loc @@ E_constant {cons_name = name; arguments = lst}

let e_cond ?loc expr match_true match_false =
  e_matching expr ?loc (Match_bool {match_true; match_false})

(*
let e_assign ?loc a b c = location_wrap ?loc @@ E_assign (Var.of_name a , b , c) (* TODO handlethat*)
*)
let ez_match_variant (lst : ((string * string) * 'a) list) =
  let lst =
    List.map (fun ((c, n), a) -> ((Constructor c, Var.of_name n), a)) lst
  in
  Match_variant (lst, ())

let e_matching_variant ?loc a (lst : ((string * string) * 'a) list) =
  e_matching ?loc a (ez_match_variant lst)

let e_record_ez ?loc (lst : (string * expr) list) : expression =
  let map =
    List.fold_left (fun m (x, y) -> LMap.add (Label x) y m) LMap.empty lst
  in
  make_expr ?loc @@ E_record map

let e_record ?loc map =
  let lst = Map.String.to_kv_list map in
  e_record_ez ?loc lst

let e_record_update ?loc record path update =
  let path = Label path in
  make_expr ?loc @@ E_record_update {record; path; update}

let e_tuple ?loc lst : expression = e_record_ez ?loc (tuple_to_record lst)

let e_pair ?loc a b : expression = e_tuple ?loc [a; b]

let make_option_typed ?loc e t_opt =
  match t_opt with None -> e | Some t -> e_annotation ?loc e t

let e_typed_none ?loc t_opt =
  let type_annotation = t_option t_opt in
  e_annotation ?loc (e_none ?loc ()) type_annotation

let e_lambda ?loc (binder : expression_variable)
    (input_type : type_expression option)
    (output_type : type_expression option) (result : expression) : expression =
  make_expr ?loc @@ E_lambda {binder; input_type; output_type; result}

let e_recursive ?loc fun_name fun_type lambda =
  make_expr ?loc @@ E_recursive {fun_name; fun_type; lambda}

let e_assign_with_let ?loc var access_path expr =
  let var = Var.of_name var in
  match access_path with
  | [] ->
      ((var, None), true, expr, false)
  | lst ->
      let rec aux path record =
        match path with
        | [] ->
            failwith "acces_path cannot be empty"
        | [e] ->
            e_record_update ?loc record e expr
        | elem :: tail ->
            let next_record = e_record_accessor record elem in
            e_record_update ?loc record elem (aux tail next_record)
      in
      ((var, None), true, aux lst (e_variable var), false)

let get_e_record_accessor t =
  match t with
  | E_record_accessor {record; path} ->
      ok (record, path)
  | _ ->
      simple_fail "not an accessor"

let assert_e_record_accessor t =
  let%bind _ = get_e_record_accessor t in
  ok ()

let get_e_pair t =
  match t with
  | E_record r -> (
      let lst = LMap.to_kv_list r in
      match lst with
      | [(Label "O", a); (Label "1", b)] | [(Label "1", b); (Label "0", a)] ->
          ok (a, b)
      | _ ->
          simple_fail "not a pair" )
  | _ ->
      simple_fail "not a pair"

let get_e_list t =
  let rec aux t =
    match t with
    | E_constant {cons_name = C_CONS; arguments = [key; lst]} ->
        let%bind lst = aux lst.expression_content in
        ok @@ (key :: lst)
    | E_constant {cons_name = C_LIST_EMPTY; arguments = []} ->
        ok @@ []
    | _ ->
        simple_fail "not a list"
  in
  aux t

let get_e_tuple t =
  match t with
  | E_record r ->
      ok @@ List.map snd @@ Stage_common.Helpers.tuple_of_record r
  | _ ->
      simple_fail "ast_core: get_e_tuple: not a tuple"

(* Same as get_e_pair *)
let extract_pair : expression -> (expression * expression) result =
 fun e ->
  match e.expression_content with
  | E_record r -> (
      let lst = LMap.to_kv_list r in
      match lst with
      | [(Label "O", a); (Label "1", b)] | [(Label "1", b); (Label "0", a)] ->
          ok (a, b)
      | _ ->
          fail @@ bad_kind "pair" e.location )
  | _ ->
      fail @@ bad_kind "pair" e.location

let extract_record : expression -> (label * expression) list result =
 fun e ->
  match e.expression_content with
  | E_record lst ->
      ok @@ LMap.to_kv_list lst
  | _ ->
      fail @@ bad_kind "record" e.location

let extract_map : expression -> (expression * expression) list result =
 fun e ->
  let rec aux e =
    match e.expression_content with
    | E_constant {cons_name = C_UPDATE | C_MAP_ADD; arguments = [k; v; map]} ->
        let%bind map = aux map in
        ok @@ ((k, v) :: map)
    | E_constant {cons_name = C_MAP_EMPTY | C_BIG_MAP_EMPTY; arguments = []} ->
        ok @@ []
    | _ ->
        fail @@ bad_kind "map" e.location
  in
  aux e
