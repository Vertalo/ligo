open Types
open Trace

let type_operator_name = function
    TC_contract         -> "TC_contract"
  | TC_option           -> "TC_option"
  | TC_list             -> "TC_list"
  | TC_set              -> "TC_set"
  | TC_map              -> "TC_map"
  | TC_big_map          -> "TC_big_map"
  | TC_map_or_big_map   -> "TC_map_or_big_map"
  | TC_michelson_pair   -> "TC_michelson_pair"
  | TC_michelson_or     -> "TC_michelson_or"
  | TC_michelson_pair_right_comb -> "TC_michelson_pair_right_comb"
  | TC_michelson_pair_left_comb  -> "TC_michelson_pair_left_comb"
  | TC_michelson_or_right_comb   -> "TC_michelson_or_right_comb"
  | TC_michelson_or_left_comb    -> "TC_michelson_or_left_comb"

let type_expression'_of_string = fun t ->
  let aux operator args = T_operator {operator; args} in
  match t with
  | "TC_contract" , [x]     -> ok @@ aux TC_contract [ x ]
  | "TC_option"   , [x]     -> ok @@ aux TC_option   [ x ]
  | "TC_list"     , [x]     -> ok @@ aux TC_list     [ x ]
  | "TC_set"      , [x]     -> ok @@ aux TC_set      [ x ]
  | "TC_map"      , [k ; v] -> ok @@ aux TC_map      [ k; v ]
  | "TC_big_map"  , [k ; v] -> ok @@ aux TC_big_map  [ k; v ]
  | ("TC_contract" | "TC_option" | "TC_list" | "TC_set" | "TC_map" | "TC_big_map"), _ ->
     failwith "internal error: wrong number of arguments for type operator"

  | "TC_unit"      , [] -> ok @@ T_constant(TC_unit)
  | "TC_string"    , [] -> ok @@ T_constant(TC_string)
  | "TC_bytes"     , [] -> ok @@ T_constant(TC_bytes)
  | "TC_nat"       , [] -> ok @@ T_constant(TC_nat)
  | "TC_int"       , [] -> ok @@ T_constant(TC_int)
  | "TC_mutez"     , [] -> ok @@ T_constant(TC_mutez)
  | "TC_operation" , [] -> ok @@ T_constant(TC_operation)
  | "TC_address"   , [] -> ok @@ T_constant(TC_address)
  | "TC_key"       , [] -> ok @@ T_constant(TC_key)
  | "TC_key_hash"  , [] -> ok @@ T_constant(TC_key_hash)
  | "TC_chain_id"  , [] -> ok @@ T_constant(TC_chain_id)
  | "TC_signature" , [] -> ok @@ T_constant(TC_signature)
  | "TC_timestamp" , [] -> ok @@ T_constant(TC_timestamp)
  | _,               [] ->
     failwith "internal error: wrong number of arguments for type constant"
  | _                       ->
     failwith "internal error: unknown type operator"

let string_of_type_operator = function
  | TC_contract       -> "TC_contract"
  | TC_option         -> "TC_option"
  | TC_list           -> "TC_list"
  | TC_set            -> "TC_set"
  | TC_map            -> "TC_map"
  | TC_big_map        -> "TC_big_map"
  | TC_map_or_big_map -> "TC_map_or_big_map"
  | TC_michelson_pair   -> "TC_michelson_pair"
  | TC_michelson_or     -> "TC_michelson_or"
  | TC_michelson_pair_right_comb -> "TC_michelson_pair_right_comb"
  | TC_michelson_pair_left_comb  -> "TC_michelson_pair_left_comb"
  | TC_michelson_or_right_comb   -> "TC_michelson_or_right_comb"
  | TC_michelson_or_left_comb    -> "TC_michelson_or_left_comb"

let string_of_type_constant = function
  | TC_unit      -> "TC_unit"
  | TC_string    -> "TC_string"
  | TC_bytes     -> "TC_bytes"
  | TC_nat       -> "TC_nat"
  | TC_int       -> "TC_int"
  | TC_mutez     -> "TC_mutez"
  | TC_operation -> "TC_operation"
  | TC_address   -> "TC_address"
  | TC_key       -> "TC_key"
  | TC_key_hash  -> "TC_key_hash"
  | TC_chain_id  -> "TC_chain_id"
  | TC_signature -> "TC_signature"
  | TC_timestamp -> "TC_timestamp"

let string_of_type_expression' = function
  | T_operator {operator; args} -> string_of_type_operator operator,args
  | T_constant c -> string_of_type_constant c,[]
  | T_sum _ | T_record _ | T_arrow _ | T_variable _ | T_existential _ ->
     failwith "not a type operator or constant"

let bind_lmap (l:_ label_map) =
  let open Trace in
  let open LMap in
  let aux k v prev =
    prev >>? fun prev' ->
    v >>? fun v' ->
    ok @@ add k v' prev' in
  fold aux l (ok empty)

let bind_fold_lmap f init (lmap:_ LMap.t) =
  let open Trace in
  let aux k v prev =
    prev >>? fun prev' ->
    f prev' k v
  in
  LMap.fold aux lmap init

let bind_map_lmap f map = bind_lmap (LMap.map f map)
let bind_map_lmap_t f map = bind_lmap (
  LMap.map 
    (fun ({associated_type;_}) -> 
      f associated_type)
    map)
let bind_map_lmapi f map = bind_lmap (LMap.mapi f map)

let range i j =
  let rec aux i j acc = if i >= j then acc else aux i (j-1) (j-1 :: acc) in
  aux i j []

let label_range i j =
  List.map (fun i -> Label (string_of_int i)) @@ range i j

let is_tuple_lmap m =
  List.for_all (fun i -> LMap.mem i m) @@ (label_range 0 (LMap.cardinal m))

let get_pair m =
  match (LMap.find_opt (Label "0") m , LMap.find_opt (Label "1") m) with
  | Some {associated_type=e1;_}, Some {associated_type=e2;_} -> Some (e1,e2)
  | _ -> None

let tuple_of_record (m: _ LMap.t) =
  let aux i = 
    let label = Label (string_of_int i) in
    let opt = LMap.find_opt (label) m in
    Option.bind (fun opt -> Some ((label,opt),i+1)) opt
  in
  Base.Sequence.to_list @@ Base.Sequence.unfold ~init:0 ~f:aux

let list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    List.map snd @@ tuple_of_record m
  else
    List.rev @@ LMap.to_list m

let kv_list_of_record_or_tuple (m: _ LMap.t) =
  if (is_tuple_lmap m) then
    tuple_of_record m
  else
    List.rev @@ LMap.to_kv_list m


let remove_empty_annotation (ann : string option) : string option =
  match ann with
  | Some "" -> None
  | Some ann -> Some (String.uncapitalize_ascii ann)
  | None -> None

let is_michelson_or (t: _ label_map) =
  LMap.cardinal t = 2 && 
  (LMap.mem (Label "M_left") t) &&
  (LMap.mem (Label "M_right") t)

let is_michelson_pair (t: _ label_map) =
  LMap.cardinal t = 2 && 
  let l = LMap.to_list t in
  List.fold_left
    (fun prev {michelson_annotation;_} -> match michelson_annotation with
      | Some _ -> true
      | None -> prev)
    false 
    l &&
  List.for_all (fun i -> LMap.mem i t) @@ (label_range 0 (LMap.cardinal t))
