open Solver
open Format

let c_tag_to_string : Solver.Core.constant_tag -> string = function
  | Solver.Core.C_arrow     -> "arrow"
  | Solver.Core.C_option    -> "option"
  | Solver.Core.C_tuple     -> "tuple"
  | Solver.Core.C_record    -> failwith "record"
  | Solver.Core.C_variant   -> failwith "variant"
  | Solver.Core.C_map       ->  "map"
  | Solver.Core.C_big_map   ->  "big_map"
  | Solver.Core.C_list      ->  "list"
  | Solver.Core.C_set       ->  "set"
  | Solver.Core.C_unit      ->  "unit"
  | Solver.Core.C_bool      ->  "bool"
  | Solver.Core.C_string    ->  "string"
  | Solver.Core.C_nat       ->  "nat"
  | Solver.Core.C_tez       ->  "mutez"
  | Solver.Core.C_timestamp ->  "timestamp"
  | Solver.Core.C_int       ->  "int"
  | Solver.Core.C_address   ->  "address"
  | Solver.Core.C_bytes     ->  "bytes"
  | Solver.Core.C_key_hash  ->  "key_hash"
  | Solver.Core.C_key       ->  "key"
  | Solver.Core.C_signature ->  "signature"
  | Solver.Core.C_operation ->  "operation"
  | Solver.Core.C_contract  ->  "contract"

let type_constraint : _ -> type_constraint_simpl -> unit = fun ppf ->
  function
  |SC_Constructor { tv; c_tag; tv_list=_ } ->
    let ct = c_tag_to_string c_tag in
    fprintf ppf "CTOR %s %s()" tv ct
  |SC_Alias       (a, b) -> fprintf ppf "Alias %s %s" a b
  |SC_Poly        _ -> fprintf ppf "Poly"
  |SC_Typeclass   _ -> fprintf ppf "TC"

let all_constraints ppf ac =
  fprintf ppf "[" ;
  pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf ";\n") type_constraint ppf ac ;
  fprintf ppf "]"

let aliases ppf (al : unionfind) =
  fprintf ppf "ALIASES %a" UF.print al

let structured_dbs : _ -> structured_dbs -> unit = fun ppf structured_dbs ->
  let { all_constraints = a ; aliases = b ; _ } = structured_dbs in
  fprintf ppf "STRUCTURED_DBS\n %a\n %a" all_constraints a aliases b

let already_selected : _ -> already_selected -> unit = fun ppf already_selected ->
  let _ = already_selected in
  fprintf ppf "ALREADY_SELECTED"

let state : _ -> state -> unit = fun ppf state ->
  let { structured_dbs=a ; already_selected=b } = state in
  fprintf ppf "STATE %a %a" structured_dbs a already_selected b
