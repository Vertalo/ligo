open Types
open Format
open PP_helpers

let constructor ppf (c:constructor') : unit =
  let Constructor c = c in fprintf ppf "%s" c

let label ppf (l:label) : unit =
  let Label l = l in fprintf ppf "%s" l


let list_sep_d x = list_sep x (tag " ,@ ")

let constant ppf : constant' -> unit = function
  | C_INT                   -> fprintf ppf "INT"
  | C_UNIT                  -> fprintf ppf "UNIT"
  | C_NIL                   -> fprintf ppf "NIL"
  | C_NOW                   -> fprintf ppf "NOW"
  | C_IS_NAT                -> fprintf ppf "IS_NAT"
  | C_SOME                  -> fprintf ppf "SOME"
  | C_NONE                  -> fprintf ppf "NONE"
  | C_ASSERTION             -> fprintf ppf "ASSERTION"
  | C_ASSERT_INFERRED       -> fprintf ppf "ASSERT_INFERRED"
  | C_FAILWITH              -> fprintf ppf "FAILWITH"
  | C_UPDATE                -> fprintf ppf "UPDATE"
  (* Loops *)
  | C_ITER                  -> fprintf ppf "ITER"
  | C_FOLD                  -> fprintf ppf "FOLD"
  | C_FOLD_WHILE            -> fprintf ppf "FOLD_WHILE"
  | C_FOLD_CONTINUE         -> fprintf ppf "CONTINUE"
  | C_FOLD_STOP             -> fprintf ppf "STOP"
  | C_LOOP_LEFT             -> fprintf ppf "LOOP_LEFT"
  | C_LOOP_CONTINUE         -> fprintf ppf "LOOP_CONTINUE"
  | C_LOOP_STOP             -> fprintf ppf "LOOP_STOP"
  (* MATH *)
  | C_NEG                   -> fprintf ppf "NEG"
  | C_ABS                   -> fprintf ppf "ABS"
  | C_ADD                   -> fprintf ppf "ADD"
  | C_SUB                   -> fprintf ppf "SUB"
  | C_MUL                   -> fprintf ppf "MUL"
  | C_EDIV                  -> fprintf ppf "EDIV"
  | C_DIV                   -> fprintf ppf "DIV"
  | C_MOD                   -> fprintf ppf "MOD"
  (* LOGIC *)
  | C_NOT                   -> fprintf ppf "NOT"
  | C_AND                   -> fprintf ppf "AND"
  | C_OR                    -> fprintf ppf "OR"
  | C_XOR                   -> fprintf ppf "XOR"
  | C_LSL                   -> fprintf ppf "LSL"
  | C_LSR                   -> fprintf ppf "LSR"
  (* COMPARATOR *)
  | C_EQ                    -> fprintf ppf "EQ"
  | C_NEQ                   -> fprintf ppf "NEQ"
  | C_LT                    -> fprintf ppf "LT"
  | C_GT                    -> fprintf ppf "GT"
  | C_LE                    -> fprintf ppf "LE"
  | C_GE                    -> fprintf ppf "GE"
  (* Bytes/ String *)
  | C_SIZE                  -> fprintf ppf "SIZE"
  | C_CONCAT                -> fprintf ppf "CONCAT"
  | C_SLICE                 -> fprintf ppf "SLICE"
  | C_BYTES_PACK            -> fprintf ppf "BYTES_PACK"
  | C_BYTES_UNPACK          -> fprintf ppf "BYTES_UNPACK"
  | C_CONS                  -> fprintf ppf "CONS"
  (* Pair *)
  | C_PAIR                  -> fprintf ppf "PAIR"
  | C_CAR                   -> fprintf ppf "CAR"
  | C_CDR                   -> fprintf ppf "CDR"
  | C_LEFT                  -> fprintf ppf "LEFT"
  | C_RIGHT                 -> fprintf ppf "RIGHT"
  (* Set *)
  | C_SET_EMPTY             -> fprintf ppf "SET_EMPTY"
  | C_SET_LITERAL           -> fprintf ppf "SET_LITERAL"
  | C_SET_ADD               -> fprintf ppf "SET_ADD"
  | C_SET_REMOVE            -> fprintf ppf "SET_REMOVE"
  | C_SET_ITER              -> fprintf ppf "SET_ITER"
  | C_SET_FOLD              -> fprintf ppf "SET_FOLD"
  | C_SET_MEM               -> fprintf ppf "SET_MEM"
  (* List *)
  | C_LIST_EMPTY            -> fprintf ppf "LIST_EMPTY"
  | C_LIST_LITERAL          -> fprintf ppf "LIST_LITERAL"
  | C_LIST_ITER             -> fprintf ppf "LIST_ITER"
  | C_LIST_MAP              -> fprintf ppf "LIST_MAP"
  | C_LIST_FOLD             -> fprintf ppf "LIST_FOLD"
  (* Maps *)
  | C_MAP                   -> fprintf ppf "MAP"
  | C_MAP_EMPTY             -> fprintf ppf "MAP_EMPTY"
  | C_MAP_LITERAL           -> fprintf ppf "MAP_LITERAL"
  | C_MAP_GET               -> fprintf ppf "MAP_GET"
  | C_MAP_GET_FORCE         -> fprintf ppf "MAP_GET_FORCE"
  | C_MAP_ADD               -> fprintf ppf "MAP_ADD"
  | C_MAP_REMOVE            -> fprintf ppf "MAP_REMOVE"
  | C_MAP_UPDATE            -> fprintf ppf "MAP_UPDATE"
  | C_MAP_ITER              -> fprintf ppf "MAP_ITER"
  | C_MAP_MAP               -> fprintf ppf "MAP_MAP"
  | C_MAP_FOLD              -> fprintf ppf "MAP_FOLD"
  | C_MAP_MEM               -> fprintf ppf "MAP_MEM"
  | C_MAP_FIND              -> fprintf ppf "MAP_FIND"
  | C_MAP_FIND_OPT          -> fprintf ppf "MAP_FIND_OP"
  (* Big Maps *)
  | C_BIG_MAP               -> fprintf ppf "BIG_MAP"
  | C_BIG_MAP_EMPTY         -> fprintf ppf "BIG_MAP_EMPTY"
  | C_BIG_MAP_LITERAL       -> fprintf ppf "BIG_MAP_LITERAL"
  (* Crypto *)
  | C_SHA256                -> fprintf ppf "SHA256"
  | C_SHA512                -> fprintf ppf "SHA512"
  | C_BLAKE2b               -> fprintf ppf "BLAKE2b"
  | C_HASH                  -> fprintf ppf "HASH"
  | C_HASH_KEY              -> fprintf ppf "HASH_KEY"
  | C_CHECK_SIGNATURE       -> fprintf ppf "CHECK_SIGNATURE"
  | C_CHAIN_ID              -> fprintf ppf "CHAIN_ID"
  (* Blockchain *)
  | C_CALL                  -> fprintf ppf "CALL"
  | C_CONTRACT              -> fprintf ppf "CONTRACT"
  | C_CONTRACT_OPT          -> fprintf ppf "CONTRACT_OPT"
  | C_CONTRACT_ENTRYPOINT   -> fprintf ppf "CONTRACT_ENTRYPOINT"
  | C_CONTRACT_ENTRYPOINT_OPT -> fprintf ppf "CONTRACT_ENTRYPOINT_OPT"
  | C_AMOUNT                -> fprintf ppf "AMOUNT"
  | C_BALANCE               -> fprintf ppf "BALANCE"
  | C_SOURCE                -> fprintf ppf "SOURCE"
  | C_SENDER                -> fprintf ppf "SENDER"
  | C_ADDRESS               -> fprintf ppf "ADDRESS"
  | C_SELF                  -> fprintf ppf "SELF"
  | C_SELF_ADDRESS          -> fprintf ppf "SELF_ADDRESS"
  | C_IMPLICIT_ACCOUNT      -> fprintf ppf "IMPLICIT_ACCOUNT"
  | C_SET_DELEGATE          -> fprintf ppf "SET_DELEGATE"
  | C_CREATE_CONTRACT       -> fprintf ppf "CREATE_CONTRACT"
  | C_CONVERT_TO_RIGHT_COMB -> fprintf ppf "CONVERT_TO_RIGHT_COMB"
  | C_CONVERT_TO_LEFT_COMB  -> fprintf ppf "CONVERT_TO_LEFT_COMB"
  | C_CONVERT_FROM_RIGHT_COMB -> fprintf ppf "CONVERT_FROM_RIGHT_COMB"
  | C_CONVERT_FROM_LEFT_COMB  -> fprintf ppf "CONVERT_FROM_LEFT_COMB"

let operation ppf (o : Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation) : unit =
  let print_option f ppf o =
    match o with
      Some (s) -> fprintf ppf "%a" f s
    | None -> fprintf ppf "None"
  in
    let open Tezos_micheline.Micheline in
  let rec prim ppf (node : (_,Memory_proto_alpha.Protocol.Alpha_context.Script.prim) node)= match node with
    | Int (l , i) -> fprintf ppf "Int (%i, %a)" l Z.pp_print i
    | String (l , s) -> fprintf ppf "String (%i, %s)" l s
    | Bytes (l, b) -> fprintf ppf "B (%i, %s)" l (Bytes.to_string b)
    | Prim (l , p , nl, a) -> fprintf ppf "P (%i, %s, %a, %a)" l
        (Memory_proto_alpha.Protocol.Michelson_v1_primitives.string_of_prim p)
        (list_sep_d prim) nl
        (list_sep_d (fun ppf s -> fprintf ppf "%s" s)) a
    | Seq (l, nl) -> fprintf ppf "S (%i, %a)" l
        (list_sep_d prim) nl
  in
  let l ppf (l: Memory_proto_alpha.Protocol.Alpha_context.Script.lazy_expr) =
    let oo = Tezos_data_encoding.Data_encoding.force_decode l in
    match oo with
      Some o -> fprintf ppf "%a" prim (Tezos_micheline.Micheline.root o)
    | None  -> fprintf ppf "Fail decoding"
  in

  let op ppf (type a) : a Memory_proto_alpha.Protocol.Alpha_context.manager_operation -> unit = function
    | Reveal (s: Tezos_protocol_environment_006_PsCARTHA__Environment.Signature.Public_key.t) -> 
      fprintf ppf "R %a" Tezos_protocol_environment_006_PsCARTHA__Environment.Signature.Public_key.pp s
    | Transaction {amount; parameters; entrypoint; destination} ->
      fprintf ppf "T {%a; %a; %s; %a}"
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp amount
        l parameters
        entrypoint
        Memory_proto_alpha.Protocol.Alpha_context.Contract.pp destination

    | Origination {delegate; script; credit; preorigination} ->
      fprintf ppf "O {%a; %a; %a; %a}" 
        (print_option Tezos_protocol_environment_006_PsCARTHA__Environment.Signature.Public_key_hash.pp) delegate
        l script.code
        Memory_proto_alpha.Protocol.Alpha_context.Tez.pp credit
        (print_option Memory_proto_alpha.Protocol.Alpha_context.Contract.pp) preorigination
        
    | Delegation so ->
      fprintf ppf "D %a" (print_option Tezos_protocol_environment_006_PsCARTHA__Environment.Signature.Public_key_hash.pp) so
  in
  let Internal_operation {source;operation;nonce} = o in
  fprintf ppf "{source: %s; operation: %a; nonce: %i"
    (Memory_proto_alpha.Protocol.Alpha_context.Contract.to_b58check source)
    op operation
    nonce

let literal ppf (l : literal) =
  match l with
  | Literal_unit -> fprintf ppf "unit"
  | Literal_int z -> fprintf ppf "%a" Z.pp_print z
  | Literal_nat z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_timestamp z -> fprintf ppf "+%a" Z.pp_print z
  | Literal_mutez z -> fprintf ppf "%amutez" Z.pp_print z
  | Literal_string s -> fprintf ppf "%a" Ligo_string.pp s
  | Literal_bytes b -> fprintf ppf "0x%a" Hex.pp (Hex.of_bytes b)
  | Literal_address s -> fprintf ppf "@%S" s
  | Literal_operation o -> fprintf ppf "Operation(%a)" operation o
  | Literal_key s -> fprintf ppf "key %s" s
  | Literal_key_hash s -> fprintf ppf "key_hash %s" s
  | Literal_signature s -> fprintf ppf "Signature %s" s
  | Literal_chain_id s -> fprintf ppf "Chain_id %s" s

let type_variable ppf (t : type_variable) : unit = fprintf ppf "%a" Var.pp t

and type_constant ppf (tc : type_constant) : unit =
let s =
    match tc with
    | TC_unit -> "unit"
    | TC_string -> "string"
    | TC_bytes -> "bytes"
    | TC_nat -> "nat"
    | TC_int -> "int"
    | TC_mutez -> "mutez"
    | TC_operation -> "operation"
    | TC_address -> "address"
    | TC_key -> "key"
    | TC_key_hash -> "key_hash"
    | TC_signature -> "signature"
    | TC_timestamp -> "timestamp"
    | TC_chain_id -> "chain_id"
in
fprintf ppf "%s" s

module Ast_PP_type (PARAMETER : AST_PARAMETER_TYPE) = struct
  module Agt=Ast_generic_type(PARAMETER)
  open Agt
  open Format

  let cmap_sep value sep ppf m =
    let lst = CMap.to_kv_list m in
    let lst = List.sort (fun (Constructor a,_) (Constructor b,_) -> String.compare a b) lst in
    let new_pp ppf (k, {ctor_type;_}) = fprintf ppf "@[<h>%a -> %a@]" constructor k value ctor_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst
  let cmap_sep_d x = cmap_sep x (tag " ,@ ")

  let record_sep value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, {field_type;_}) = fprintf ppf "@[<h>%a -> %a@]" label k value field_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let tuple_sep value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_, {field_type;_}) = fprintf ppf "%a" value field_type in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let record_sep_expr value sep ppf (m : 'a label_map) =
    let lst = LMap.to_kv_list m in
    let lst = List.sort_uniq (fun (Label a,_) (Label b,_) -> String.compare a b) lst in
    let new_pp ppf (k, v) = fprintf ppf "@[<h>%a -> %a@]" label k value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  let tuple_sep_expr value sep ppf m =
    assert (Helpers.is_tuple_lmap m);
    let lst = Helpers.tuple_of_record m in
    let new_pp ppf (_,v) = fprintf ppf "%a" value v in
    fprintf ppf "%a" (list_sep new_pp sep) lst

  (* Prints records which only contain the consecutive fields
    0..(cardinal-1) as tuples *)
  let tuple_or_record_sep_t value format_record sep_record format_tuple sep_tuple ppf m =
    if Helpers.is_tuple_lmap m then
      fprintf ppf format_tuple (tuple_sep value (tag sep_tuple)) m
    else
      fprintf ppf format_record (record_sep value (tag sep_record)) m

  let tuple_or_record_sep_expr value format_record sep_record format_tuple sep_tuple ppf m =
    if Helpers.is_tuple_lmap m then
      fprintf ppf format_tuple (tuple_sep_expr value (tag sep_tuple)) m
    else
      fprintf ppf format_record (record_sep_expr value (tag sep_record)) m

  let tuple_or_record_sep_expr value = tuple_or_record_sep_expr value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " ,@ "
  let tuple_or_record_sep_type value = tuple_or_record_sep_t value "@[<hv 7>record[%a]@]" " ,@ " "@[<hv 2>( %a )@]" " *@ "

  let rec type_expression' :
         (formatter -> type_expression -> unit)
      -> formatter
      -> type_expression
      -> unit =
   fun f ppf te ->
    match te.type_content with
    | T_sum m -> fprintf ppf "@[<hv 4>sum[%a]@]" (cmap_sep_d f) m
    | T_record m -> fprintf ppf "%a" (tuple_or_record_sep_type f) m
    | T_arrow a -> fprintf ppf "%a -> %a" f a.type1 f a.type2
    | T_variable tv -> type_variable ppf tv
    | T_constant tc -> type_constant ppf tc
    | T_operator to_ -> type_operator f ppf to_

  and type_expression ppf (te : type_expression) : unit =
    type_expression' type_expression ppf te

  and type_operator : (formatter -> type_expression -> unit) -> formatter -> type_operator * type_expression list -> unit =
   fun f ppf to_ ->
    let s = match to_ with
      TC_option                    , lst -> Format.asprintf "option(%a)"                     (list_sep_d f) lst
    | TC_list                      , lst -> Format.asprintf "list(%a)"                       (list_sep_d f) lst
    | TC_set                       , lst -> Format.asprintf "set(%a)"                        (list_sep_d f) lst
    | TC_map                       , lst -> Format.asprintf "Map (%a)"                       (list_sep_d f) lst
    | TC_big_map                   , lst -> Format.asprintf "Big Map (%a)"                   (list_sep_d f) lst
    | TC_map_or_big_map            , lst -> Format.asprintf "Map Or Big Map (%a)"            (list_sep_d f) lst
    | TC_contract                  , lst -> Format.asprintf "Contract (%a)"                  (list_sep_d f) lst
    | TC_michelson_pair            , lst -> Format.asprintf "michelson_pair (%a)"            (list_sep_d f) lst                            
    | TC_michelson_or              , lst -> Format.asprintf "michelson_or (%a)"              (list_sep_d f) lst
    | TC_michelson_pair_right_comb , lst -> Format.asprintf "michelson_pair_right_comb (%a)" (list_sep_d f) lst
    | TC_michelson_pair_left_comb  , lst -> Format.asprintf "michelson_pair_left_comb (%a)"  (list_sep_d f) lst
    | TC_michelson_or_right_comb   , lst -> Format.asprintf "michelson_or_right_comb (%a)"   (list_sep_d f) lst
    | TC_michelson_or_left_comb    , lst -> Format.asprintf "michelson_or_left_comb (%a)"    (list_sep_d f) lst
    in
    fprintf ppf "(type_operator: %s)" s
end
