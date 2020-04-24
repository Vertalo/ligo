open Trace

(*
  This file is used throughout the pipeline. Its idea is to add a unique place
  that you have to modify when you add a new operator/constant to the language.

  This file mirrors the LIGO pipeline, starting with Simplify, then Typer and
  ending with Compiler. Usually, when adding a new operator, you'll have to add
  a new constructor at all those places.
*)

module Concrete_to_imperative = struct

  open Ast_imperative
  (*
    Each front-end has its owns constants.

    Constants are special names that have their own case in the AST. E_constant
    for regular constants, and T_constant for type constants. Both types are
    defined in `Ast_core/types.ml`.
    For instance, "2 + 2" in Pascaligo is translated to `E_constant ("ADD" , [
      E_literal (Literal_int 2) ;
      E_literal (Literal_int 2) ;
    ])`.

    They are used to represent what can't expressed in the languages:
    - Primitives. Like "int", "string", "unit" for types. Or "+" for values.
    - Tezos specific stuff. Like "operation" for types. Or "source" for values.
    - What can't be represented in the language yet. Like "list" or "List.fold".

    Each constant is expressed as a pair:
    - The left-hand-side is the reserved name in the given front-end.
    - The right-hand-side is the name that will be used in the AST.
  *)
  let unit_expr = make_t @@ T_constant TC_unit

  let type_constants s =
    match s with
      "chain_id"  -> Some TC_chain_id
    | "unit"      -> Some TC_unit
    | "string"    -> Some TC_string
    | "bytes"     -> Some TC_bytes
    | "nat"       -> Some TC_nat
    | "int"       -> Some TC_int
    | "tez"       -> Some TC_mutez
    | "bool"      -> Some TC_bool
    | "operation" -> Some TC_operation
    | "address"   -> Some TC_address
    | "key"       -> Some TC_key
    | "key_hash"  -> Some TC_key_hash
    | "signature" -> Some TC_signature
    | "timestamp" -> Some TC_timestamp
    | _           -> None

  let type_operators s =
    match s with
      "list"         -> Some (TC_list unit_expr)
    | "option"       -> Some (TC_option unit_expr)
    | "set"          -> Some (TC_set unit_expr)
    | "map"          -> Some (TC_map (unit_expr,unit_expr))
    | "big_map"      -> Some (TC_big_map (unit_expr,unit_expr))
    | "michelson_or" -> Some (TC_michelson_or (unit_expr,"",unit_expr,""))
    | "contract"     -> Some (TC_contract unit_expr)
    | _              -> None

  let pseudo_modules = function
    | "Tezos.chain_id"           -> Some C_CHAIN_ID
    | "Tezos.balance"            -> Some C_BALANCE
    | "Tezos.now"                -> Some C_NOW
    | "Tezos.amount"             -> Some C_AMOUNT
    | "Tezos.sender"             -> Some C_SENDER
    | "Tezos.address"            -> Some C_ADDRESS
    | "Tezos.self"               -> Some C_SELF
    | "Tezos.self_address"       -> Some C_SELF_ADDRESS
    | "Tezos.implicit_account"   -> Some C_IMPLICIT_ACCOUNT
    | "Tezos.source"             -> Some C_SOURCE
    | "Tezos.failwith"           -> Some C_FAILWITH
    | "Tezos.create_contract"    -> Some C_CREATE_CONTRACT
    | "Tezos.transaction"        -> Some C_CALL
    | "Tezos.set_delegate"       -> Some C_SET_DELEGATE
    | "Tezos.get_contract_opt"   -> Some C_CONTRACT_OPT
    | "Tezos.get_entrypoint_opt" -> Some C_CONTRACT_ENTRYPOINT_OPT

    (* Crypto module *)

    | "Crypto.check"    -> Some C_CHECK_SIGNATURE
    | "Crypto.hash_key" -> Some C_HASH_KEY    
    | "Crypto.blake2b"  -> Some C_BLAKE2b
    | "Crypto.sha256"   -> Some C_SHA256    
    | "Crypto.sha512"   -> Some C_SHA512
    
    (* Bytes module *)

    | "Bytes.pack"   -> Some C_BYTES_PACK    
    | "Bytes.unpack" -> Some C_BYTES_UNPACK
    | "Bytes.length" -> Some C_SIZE    
    | "Bytes.concat" -> Some C_CONCAT
    | "Bytes.sub"    -> Some C_SLICE

    (* List module *)   

    | "List.length" -> Some C_SIZE
    | "List.size"   -> Some C_SIZE
    | "List.iter"   -> Some C_LIST_ITER
    | "List.map"    -> Some C_LIST_MAP
    | "List.fold"   -> Some C_LIST_FOLD

    (* Set module *)   

    | "Set.empty"    -> Some C_SET_EMPTY
    | "Set.literal"  -> Some C_SET_LITERAL
    | "Set.cardinal" -> Some C_SIZE
    | "Set.mem"      -> Some C_SET_MEM
    | "Set.add"      -> Some C_SET_ADD
    | "Set.remove"   -> Some C_SET_REMOVE
    | "Set.iter"     -> Some C_SET_ITER
    | "Set.fold"     -> Some C_SET_FOLD

    (* Map module *)   

    | "Map.find_opt" -> Some C_MAP_FIND_OPT
    | "Map.update"   -> Some C_MAP_UPDATE
    | "Map.iter"     -> Some C_MAP_ITER
    | "Map.map"      -> Some C_MAP_MAP
    | "Map.fold"     -> Some C_MAP_FOLD
    | "Map.mem"      -> Some C_MAP_MEM
    | "Map.size"     -> Some C_SIZE
    | "Map.add"      -> Some C_MAP_ADD
    | "Map.remove"   -> Some C_MAP_REMOVE
    | "Map.empty"    -> Some C_MAP_EMPTY
    | "Map.literal"  -> Some C_MAP_LITERAL
    
    (* Big_map module *)   
    
    | "Big_map.find"     -> Some C_MAP_FIND
    | "Big_map.find_opt" -> Some C_MAP_FIND_OPT
    | "Big_map.update"   -> Some C_MAP_UPDATE
    | "Big_map.literal"  -> Some C_BIG_MAP_LITERAL
    | "Big_map.empty"    -> Some C_BIG_MAP_EMPTY
    | "Big_map.mem"      -> Some C_MAP_MEM
    | "Big_map.remove"   -> Some C_MAP_REMOVE
    | "Big_map.add"      -> Some C_MAP_ADD

    (* Bitwise module *)

    | "Bitwise.or"          -> Some C_OR
    | "Bitwise.and"         -> Some C_AND
    | "Bitwise.xor"         -> Some C_XOR
    | "Bitwise.shift_left"  -> Some C_LSL
    | "Bitwise.shift_right" -> Some C_LSR

    (* String module *)

    | "String.length"   -> Some C_SIZE
    | "String.size"     -> Some C_SIZE  (* Deprecated *)
    | "String.slice"    -> Some C_SLICE (* Deprecated *)
    | "String.sub"      -> Some C_SLICE
    | "String.concat"   -> Some C_CONCAT

    | _ -> None


  module Pascaligo = struct
    let constants = function
    (* Tezos module (ex-Michelson) *)
    | "chain_id"               -> Some C_CHAIN_ID            (* Deprecated *)
    | "get_chain_id"           -> Some C_CHAIN_ID            (* Deprecated *)    
    | "balance"                -> Some C_BALANCE             (* Deprecated *)    
    | "now"                    -> Some C_NOW                 (* Deprecated *)
    | "amount"                 -> Some C_AMOUNT              (* Deprecated *)
    | "sender"                 -> Some C_SENDER              (* Deprecated *)
    | "address"                -> Some C_ADDRESS             (* Deprecated *)    
    | "self_address"           -> Some C_SELF_ADDRESS        (* Deprecated *)
    | "implicit_account"       -> Some C_IMPLICIT_ACCOUNT    (* Deprecated *)    
    | "source"                 -> Some C_SOURCE              (* Deprecated *)    
    | "failwith"               -> Some C_FAILWITH
    | "transaction"            -> Some C_CALL                    (* Deprecated *)
    | "set_delegate"           -> Some C_SET_DELEGATE            (* Deprecated *)
    | "get_contract"           -> Some C_CONTRACT                (* Deprecated *)
    | "get_contract_opt"       -> Some C_CONTRACT_OPT            (* Deprecated *)
    | "get_entrypoint"         -> Some C_CONTRACT_ENTRYPOINT     (* Deprecated *)
    | "get_entrypoint_opt"     -> Some C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

    | "Michelson.is_nat" -> Some C_IS_NAT  (* Deprecated *)
    | "is_nat"           -> Some C_IS_NAT
    | "int"              -> Some C_INT
    | "abs"              -> Some C_ABS
    | "ediv"             -> Some C_EDIV
    | "unit"             -> Some C_UNIT

    | "NEG"              -> Some C_NEG
    | "ADD"              -> Some C_ADD
    | "SUB"              -> Some C_SUB
    | "TIMES"            -> Some C_MUL
    | "DIV"              -> Some C_DIV
    | "MOD"              -> Some C_MOD
    | "EQ"               -> Some C_EQ
    | "NOT"              -> Some C_NOT
    | "AND"              -> Some C_AND
    | "OR"               -> Some C_OR
    | "GT"               -> Some C_GT
    | "GE"               -> Some C_GE
    | "LT"               -> Some C_LT
    | "LE"               -> Some C_LE
    | "CONS"             -> Some C_CONS
    | "cons"             -> Some C_CONS (* Deprecated *)
    | "NEQ"              -> Some C_NEQ

    (* Crypto module *)

    | "crypto_check"    -> Some C_CHECK_SIGNATURE       (* Deprecated *)
    | "crypto_hash_key" -> Some C_HASH_KEY              (* Deprecated *)    
    | "blake2b"         -> Some C_BLAKE2b               (* Deprecated *)    
    | "sha_256"         -> Some C_SHA256                (* Deprecated *)       
    | "sha_512"         -> Some C_SHA512                (* Deprecated *)

    (* Bytes module *)

    | "bytes_pack"   -> Some C_BYTES_PACK    (* Deprecated *)    
    | "bytes_unpack" -> Some C_BYTES_UNPACK  (* Deprecated *)    
    | "Bytes.size"   -> Some C_SIZE          (* Deprecated *)
    | "bytes_concat" -> Some C_CONCAT        (* Deprecated *)    
    | "bytes_slice"  -> Some C_SLICE         (* Deprecated *)
    | "Bytes.slice"  -> Some C_SLICE         (* Deprecated *)

    (* List module *)

    | "list_size"   -> Some C_SIZE       (* Deprecated *)
    | "list_iter"   -> Some C_LIST_ITER  (* Deprecated *)    
    | "list_map"    -> Some C_LIST_MAP   (* Deprecated *)    
    | "list_fold"   -> Some C_LIST_FOLD  (* Deprecated *)

    (* Set module *)

    
    | "Set.size"    -> Some C_SIZE        (* Deprecated *)
    | "set_size"    -> Some C_SIZE        (* Deprecated *)
    | "set_empty"   -> Some C_SET_EMPTY   (* Deprecated *)    
    | "set_mem"     -> Some C_SET_MEM     (* Deprecated *)    
    | "set_add"     -> Some C_SET_ADD     (* Deprecated *)    
    | "set_remove"  -> Some C_SET_REMOVE  (* Deprecated *)    
    | "set_iter"    -> Some C_SET_ITER    (* Deprecated *)    
    | "set_fold"    -> Some C_SET_FOLD    (* Deprecated *)

    (* Map module *)

    | "get_force"    -> Some C_MAP_FIND      (* Deprecated *)
    | "map_get"      -> Some C_MAP_FIND_OPT  (* Deprecated *)    
    | "map_update"   -> Some C_MAP_UPDATE    (* Deprecated *)
    | "map_remove"   -> Some C_MAP_REMOVE    (* Deprecated *)    
    | "map_iter"     -> Some C_MAP_ITER      (* Deprecated *)    
    | "map_map"      -> Some C_MAP_MAP       (* Deprecated *)    
    | "map_fold"     -> Some C_MAP_FOLD      (* Deprecated *)    
    | "map_mem"      -> Some C_MAP_MEM       (* Deprecated *)    
    | "map_size"     -> Some C_SIZE          (* Deprecated *)


    (* Bitwise module *)

    | "bitwise_or"          -> Some C_OR      (* Deprecated *)    
    | "bitwise_and"         -> Some C_AND     (* Deprecated *)    
    | "bitwise_xor"         -> Some C_XOR     (* Deprecated *)    
    | "bitwise_lsl"         -> Some C_LSL     (* Deprecated *)    
    | "bitwise_lsr"         -> Some C_LSR     (* Deprecated *)

    (* String module *)
    
    | "string_slice"    -> Some C_SLICE    (* Deprecated *)
    | "string_concat"   -> Some C_CONCAT   (* Deprecated *)

    (* Others *)

    | "assert"          -> Some C_ASSERTION
    | "size"            -> Some C_SIZE (* Deprecated *)

    | _ as c            -> pseudo_modules c

    let type_constants = type_constants
    let type_operators = type_operators
  end

  module Cameligo = struct
    let constants = function
    (* Tezos (ex-Michelson, ex-Current, ex-Operation) *)

    | "chain_id"                   -> Some C_CHAIN_ID            (* Deprecated *)    
    | "Current.balance"            -> Some C_BALANCE             (* Deprecated *)
    | "balance"                    -> Some C_BALANCE             (* Deprecated *)    
    | "Current.time"               -> Some C_NOW                 (* Deprecated *)
    | "time"                       -> Some C_NOW                 (* Deprecated *)    
    | "Current.amount"             -> Some C_AMOUNT              (* Deprecated *)
    | "amount"                     -> Some C_AMOUNT              (* Deprecated *)    
    | "Current.sender"             -> Some C_SENDER              (* Deprecated *)
    | "sender"                     -> Some C_SENDER              (* Deprecated *)    
    | "Current.address"            -> Some C_ADDRESS             (* Deprecated *)    
    | "Current.self_address"       -> Some C_SELF_ADDRESS        (* Deprecated *)    
    | "Current.implicit_account"   -> Some C_IMPLICIT_ACCOUNT    (* Deprecated *)    
    | "Current.source"             -> Some C_SOURCE              (* Deprecated *)
    | "source"                     -> Some C_SOURCE              (* Deprecated *)    
    | "Current.failwith"           -> Some C_FAILWITH            (* Deprecated *)
    | "failwith"                   -> Some C_FAILWITH
    
    | "Operation.transaction"        -> Some C_CALL              (* Deprecated *)
    | "Operation.set_delegate"       -> Some C_SET_DELEGATE      (* Deprecated *)
    | "Operation.get_contract"       -> Some C_CONTRACT          (* Deprecated *)
    | "Operation.get_contract_opt"   -> Some C_CONTRACT_OPT      (* Deprecated *)
    | "Operation.get_entrypoint"     -> Some C_CONTRACT_ENTRYPOINT (* Deprecated *)
    | "Operation.get_entrypoint_opt" -> Some C_CONTRACT_ENTRYPOINT_OPT (* Deprecated *)

    | "Michelson.is_nat" -> Some C_IS_NAT  (* Deprecated *)
    | "is_nat"           -> Some C_IS_NAT
    | "int"              -> Some C_INT
    | "abs"              -> Some C_ABS
    | "ediv"             -> Some C_EDIV
    | "unit"             -> Some C_UNIT

    | "NEG"              -> Some C_NEG
    | "ADD"              -> Some C_ADD
    | "SUB"              -> Some C_SUB
    | "TIMES"            -> Some C_MUL
    | "DIV"              -> Some C_DIV
    | "MOD"              -> Some C_MOD
    | "EQ"               -> Some C_EQ
    | "NOT"              -> Some C_NOT
    | "AND"              -> Some C_AND
    | "OR"               -> Some C_OR
    | "GT"               -> Some C_GT
    | "GE"               -> Some C_GE
    | "LT"               -> Some C_LT
    | "LE"               -> Some C_LE
    | "CONS"             -> Some C_CONS
    | "NEQ"              -> Some C_NEQ

    (* Bytes module *)

    | "Bytes.size"   -> Some C_SIZE       (* Deprecated *)
    | "Bytes.slice"  -> Some C_SLICE      (* Deprecated *)

    (* Set module *)   
    | "Set.size"     -> Some C_SIZE (* Deprecated *)

    (* Map module *)
    | "Map.find"     -> Some C_MAP_FIND     (* Deprecated *)

    (* Bitwise module *)

    | "Bitwise.lor"         -> Some C_OR  (* Deprecated *)
    | "Bitwise.land"        -> Some C_AND (* Deprecated *)
    | "Bitwise.lxor"        -> Some C_XOR (* Deprecated *)

    (* Loop module *)

    | "Loop.fold_while" -> Some C_FOLD_WHILE    (* Deprecated *)
    | "Loop.resume"     -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "continue"        -> Some C_FOLD_CONTINUE (* Deprecated *)
    | "Loop.stop"       -> Some C_FOLD_STOP     (* Deprecated *)
    | "stop"            -> Some C_FOLD_STOP     (* Deprecated *)

    (* Others *)

    | "assert" -> Some C_ASSERTION

    | _ as c -> pseudo_modules c

    let type_constants = type_constants
    let type_operators = type_operators
  end

end

module Typer = struct
  module Operator_errors = struct
    let type_error msg expected_type actual_type () =
      let message () =
        Format.asprintf "Expected an expression of type %a but got an expression of type %a"
          Ast_typed.PP.type_expression expected_type
          Ast_typed.PP.type_expression actual_type in
      error (thunk msg) message

    open PP_helpers

    let print_f_args f printer ppf args =
      Format.fprintf ppf "%s(%a)" f (list_sep printer (const " , ")) args

    (* These are handled by typeclasses in the new typer *)
    let typeclass_error msg f expected_types actual_types () =
      let message () =
        Format.asprintf "Expected arguments with one of the following combinations of types: %a but got this combination instead: %a"
          (list_sep (print_f_args f Ast_typed.PP.type_expression) (const " or ")) expected_types
          (print_f_args f Ast_typed.PP.type_expression) actual_types in
      error (thunk msg) message
  end
  (*
    Each constant has its own type.

    LIGO's type-system is currently too
    weak to express the constant's type. For instance:
    - "ADD" has a special kind of type of polymorphism. If "ADD" gets two `int`s,
      it will return an `int`. If it gets two `nat`s, it will return a `nat`.
      Regular polymorphism wouldn't work because "ADD" only accepts `int`s or
      `nat`s.
    - "NONE" (from Some/None) requires an annotation.

    Instead of a LIGO type, constant types are representend as functions. These
    functions take as parameters:
    - The list of types of the arguments of the constants. When typing `2 + 2`,
      the types might be `[ int ; int ]`.
    - The expected type of the whole expression. It is optional. When typing
      `[] : list(operation)`, it will be `Some ( list (operation) )`. When
      typing `2 + 2` (with no additional context), it will be `None`.
    The output is the type of the whole expression. An error is returned through
    the Trace monad if it doesn't type-check (`"toto" + 42`).

    Various helpers are defined and explaines in `Helpers.Typer`.
  *)

  open Helpers.Typer
  open Ast_typed

  module Operators_types = struct
    open Typesystem.Shorthands

    let tc_subarg   a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_sizearg  a     = tc [a]     [ [int] ]
    let tc_packable a     = tc [a]     [ [int] ; [string] ; [bool] (*TODO…*) ]
    let tc_timargs  a b c = tc [a;b;c] [ [nat;nat;nat] ; [int;int;int] (*TODO…*) ]
    let tc_edivargs a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_divargs  a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_modargs  a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_addargs  a b c = tc [a;b;c] [ (*TODO…*) ]
    let tc_comparable a   = tc [a]     [ [nat] ; [int] ; [mutez] ; [timestamp] ]
    let tc_concatable a   = tc [a]     [ [string] ; [bytes] ]
    let tc_storable a     = tc [a]     [ [string] ; [bytes] ; (*Humm .. TODO ?*) ]

    let t_none         = forall "a" @@ fun a -> option a

    let t_sub          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_subarg a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_some         = forall "a" @@ fun a -> a --> option a
    let t_map_remove   = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> map src dst
    let t_map_add      = forall2 "src" "dst" @@ fun src dst -> tuple3 src dst (map src dst) --> map src dst
    let t_map_update   = forall2 "src" "dst" @@ fun src dst -> tuple3 src (option dst) (map src dst) --> map src dst
    let t_map_mem      = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> bool
    let t_map_find     = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
    let t_map_find_opt = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> option dst
    let t_map_fold     = forall3 "src" "dst" "acc" @@ fun src dst acc -> tuple3 ( ( (src * dst) * acc ) --> acc ) (map src dst) acc --> acc
    let t_map_map      = forall3 "k" "v" "result" @@ fun k v result -> tuple2 ((k * v) --> result) (map k v) --> map k result

    (* TODO: the type of map_map_fold might be wrong, check it. *)
    let t_map_map_fold = forall4 "k" "v" "acc" "dst" @@ fun k v acc dst -> tuple3 ( ((k * v) * acc) --> acc * dst ) (map k v) (k * v) --> (map k dst * acc)
    let t_map_iter     = forall2 "k" "v" @@ fun k v -> tuple2 ( (k * v) --> unit ) (map k v) --> unit
    let t_size         = forall_tc "c" @@ fun c -> [tc_sizearg c] => tuple1 c --> nat (* TYPECLASS *)
    let t_slice        = tuple3 nat nat string --> string
    let t_failwith     = tuple1 string --> unit
    let t_get_force    = forall2 "src" "dst" @@ fun src dst -> tuple2 src (map src dst) --> dst
    let t_int          = tuple1 nat --> int
    let t_bytes_pack   = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 a --> bytes (* TYPECLASS *)
    let t_bytes_unpack = forall_tc "a" @@ fun a -> [tc_packable a] => tuple1 bytes --> a (* TYPECLASS *)
    let t_hash256      = tuple1 bytes --> bytes
    let t_hash512      = tuple1 bytes --> bytes
    let t_blake2b      = tuple1 bytes --> bytes
    let t_hash_key     = tuple1 key --> key_hash
    let t_is_nat       = tuple1 int --> bool
    let t_check_signature = tuple3 key signature bytes --> bool
    let t_chain_id     = tuple0 --> chain_id
    let t_sender       = tuple0 --> address
    let t_source       = tuple0 --> address
    let t_unit         = tuple0 --> unit
    let t_amount       = tuple0 --> mutez
    let t_balance      = tuple0 --> mutez
    let t_address      = tuple0 --> address
    let t_now          = tuple0 --> timestamp
    let t_transaction  = forall "a" @@ fun a -> tuple3 a mutez (contract a) --> operation
    let t_get_contract = forall "a" @@ fun a -> tuple0 --> contract a
    let t_abs          = tuple1 int --> nat
    let t_cons         = forall "a" @@ fun a -> tuple2 a (list a) --> list a
    let t_assertion    = tuple1 bool --> unit
    let t_times        = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_timargs a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_ediv         = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_edivargs a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_div          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_divargs a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_mod          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_modargs a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_add          = forall3_tc "a" "b" "c" @@ fun a b c -> [tc_addargs a b c] => tuple2 a b --> c (* TYPECLASS *)
    let t_set_mem      = forall "a" @@ fun a -> tuple2 a (set a) --> bool
    let t_set_add      = forall "a" @@ fun a -> tuple2 a (set a) --> set a
    let t_set_remove   = forall "a" @@ fun a -> tuple2 a (set a) --> set a
    let t_not          = tuple1 bool --> bool

    let t_continuation = forall "a" @@ fun a -> tuple2 bool a --> pair bool a
    let t_fold_while   = forall "a" @@ fun a -> tuple2 (a --> pair bool a) a --> a
    let t_neg          = tuple1 int --> int
    let t_and          = tuple2 bool bool --> bool
    let t_or           = tuple2 bool bool --> bool
    let t_xor          = tuple2 bool bool --> bool
    let t_lsl          = tuple2 nat nat --> nat
    let t_lsr          = tuple2 nat nat --> nat
    let t_comp         = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 a a --> bool
    let t_concat       = forall_tc "a" @@ fun a -> [tc_concatable a] => tuple2 a a --> a
    let t_set_empty    = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple0 --> set a
    let t_set_iter     = forall_tc "a" @@ fun a -> [tc_comparable a] => tuple2 (a --> unit) (set a) --> unit
    (* TODO: check that the implementation has this type *)
    let t_set_fold     = forall2_tc "a" "b" @@ fun a b -> [tc_comparable b] => tuple3 (pair a b --> a) (set b) a --> a
    let t_list_iter    = forall "a" @@ fun a -> tuple2 (a --> unit) (list a) --> unit
    let t_list_map     = forall "a" @@ fun a -> tuple2 (a --> a) (list a) --> (list a)
    (* TODO: check that the implementation has this type *)
    let t_list_fold    = forall2 "a" "b" @@ fun a b -> tuple3 (pair a b --> a) (list b) a --> a
    let t_self_address = tuple0 --> address
    let t_implicit_account = forall_tc "a" @@ fun a -> [tc_storable a] => tuple1 key_hash --> contract a
    let t_set_delegate = tuple1 (option key_hash) --> operation

    let constant_type : constant' -> Typesystem.Core.type_value result = function
      | C_INT                 -> ok @@ t_int ;
      | C_UNIT                -> ok @@ t_unit ;
      | C_NOW                 -> ok @@ t_now ;
      | C_IS_NAT              -> ok @@ t_is_nat ;
      | C_SOME                -> ok @@ t_some ;
      | C_NONE                -> ok @@ t_none ;
      | C_ASSERTION           -> ok @@ t_assertion ;
      | C_FAILWITH            -> ok @@ t_failwith ;
      (* LOOPS *)
      | C_FOLD_WHILE          -> ok @@ t_fold_while ;
      | C_FOLD_CONTINUE       -> ok @@ t_continuation ;
      | C_FOLD_STOP           -> ok @@ t_continuation ;
      (* MATH *)
      | C_NEG                 -> ok @@ t_neg ;
      | C_ABS                 -> ok @@ t_abs ;
      | C_ADD                 -> ok @@ t_add ;
      | C_SUB                 -> ok @@ t_sub ;
      | C_MUL                 -> ok @@ t_times ;
      | C_EDIV                -> ok @@ t_ediv ;
      | C_DIV                 -> ok @@ t_div ;
      | C_MOD                 -> ok @@ t_mod ;
      (* LOGIC *)
      | C_NOT                 -> ok @@ t_not ;
      | C_AND                 -> ok @@ t_and ;
      | C_OR                  -> ok @@ t_or ;
      | C_XOR                 -> ok @@ t_xor ;
      | C_LSL                 -> ok @@ t_lsl ;
      | C_LSR                 -> ok @@ t_lsr ;
      (* COMPARATOR *)
      | C_EQ                  -> ok @@ t_comp ;
      | C_NEQ                 -> ok @@ t_comp ;
      | C_LT                  -> ok @@ t_comp ;
      | C_GT                  -> ok @@ t_comp ;
      | C_LE                  -> ok @@ t_comp ;
      | C_GE                  -> ok @@ t_comp ;
      (* BYTES / STRING *)
      | C_SIZE                -> ok @@ t_size ;
      | C_CONCAT              -> ok @@ t_concat ;
      | C_SLICE               -> ok @@ t_slice ;
      | C_BYTES_PACK          -> ok @@ t_bytes_pack ;
      | C_BYTES_UNPACK        -> ok @@ t_bytes_unpack ;
      | C_CONS                -> ok @@ t_cons ;
      (* SET  *)
      | C_SET_EMPTY           -> ok @@ t_set_empty ;
      | C_SET_ADD             -> ok @@ t_set_add ;
      | C_SET_REMOVE          -> ok @@ t_set_remove ;
      | C_SET_ITER            -> ok @@ t_set_iter ;
      | C_SET_FOLD            -> ok @@ t_set_fold ;
      | C_SET_MEM             -> ok @@ t_set_mem ;

      (* LIST *)
      | C_LIST_ITER           -> ok @@ t_list_iter ;
      | C_LIST_MAP            -> ok @@ t_list_map ;
      | C_LIST_FOLD           -> ok @@ t_list_fold ;

      (* MAP *)
      | C_MAP_ADD             -> ok @@ t_map_add ;
      | C_MAP_REMOVE          -> ok @@ t_map_remove ;
      | C_MAP_UPDATE          -> ok @@ t_map_update ;
      | C_MAP_ITER            -> ok @@ t_map_iter ;
      | C_MAP_MAP             -> ok @@ t_map_map ;
      | C_MAP_FOLD            -> ok @@ t_map_fold ;
      | C_MAP_MEM             -> ok @@ t_map_mem ;
      | C_MAP_FIND            -> ok @@ t_map_find ;
      | C_MAP_FIND_OPT        -> ok @@ t_map_find_opt ;
      (* BIG MAP *)
      (* CRYPTO *)
      | C_SHA256              -> ok @@ t_hash256 ;
      | C_SHA512              -> ok @@ t_hash512 ;
      | C_BLAKE2b             -> ok @@ t_blake2b ;
      | C_HASH_KEY            -> ok @@ t_hash_key ;
      | C_CHECK_SIGNATURE     -> ok @@ t_check_signature ;
      | C_CHAIN_ID            -> ok @@ t_chain_id ;
      (*BLOCKCHAIN *)
      | C_CONTRACT            -> ok @@ t_get_contract ;
      | C_CONTRACT_ENTRYPOINT -> ok @@ failwith "t_get_entrypoint" ;
      | C_AMOUNT              -> ok @@ t_amount ;
      | C_BALANCE             -> ok @@ t_balance ;
      | C_CALL                -> ok @@ t_transaction ;
      | C_SENDER              -> ok @@ t_sender ;
      | C_SOURCE              -> ok @@ t_source ;
      | C_ADDRESS             -> ok @@ t_address ;
      | C_SELF_ADDRESS        -> ok @@ t_self_address;
      | C_IMPLICIT_ACCOUNT    -> ok @@ t_implicit_account;
      | C_SET_DELEGATE        -> ok @@ t_set_delegate ;
      | c                     -> simple_fail @@ Format.asprintf "Typer not implemented for consant %a" Ast_typed.PP.constant c
  end

  let none = typer_0 "NONE" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped NONE"
    | Some t -> ok t

  let set_empty = typer_0 "SET_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped SET_EMPTY"
    | Some t -> ok t

  let sub = typer_2 "SUB" @@ fun a b ->
    if (eq_1 a (t_int ()) || eq_1 a (t_nat ()))
    && (eq_1 b (t_int ()) || eq_1 b (t_nat ()))
    then ok @@ t_int () else
    if (eq_2 (a , b) (t_timestamp ()))
    then ok @@ t_int () else
    if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ()))
    then ok @@ t_timestamp () else
    if (eq_2 (a , b) (t_mutez ()))
    then ok @@ t_mutez () else
      fail (simple_error "Typing substraction, bad parameters.")

  let some = typer_1 "SOME" @@ fun a -> ok @@ t_option a ()

  let map_remove : typer = typer_2 "MAP_REMOVE" @@ fun k m ->
    let%bind (src , _) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src , k) in
    ok m

  let map_empty = typer_0 "MAP_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped MAP_EMPTY"
    | Some t -> 
      let%bind (src, dst) = get_t_map t in
      ok @@ t_map src dst ()

  let big_map_empty = typer_0 "BIG_MAP_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped BIG_MAP_EMPTY"
    | Some t -> 
      let%bind (src, dst) = get_t_big_map t in
      ok @@ t_big_map src dst ()

  let map_add : typer = typer_3 "MAP_ADD" @@ fun k v m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src, k) in
    let%bind () = assert_type_expression_eq (dst, v) in
    ok m

  let map_update : typer = typer_3 "MAP_UPDATE" @@ fun k v m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src, k) in
    let%bind v' = get_t_option v in
    let%bind () = assert_type_expression_eq (dst, v') in
    ok m

  let map_mem : typer = typer_2 "MAP_MEM" @@ fun k m ->
    let%bind (src, _dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src, k) in
    ok @@ t_bool ()

  let map_find : typer = typer_2 "MAP_FIND" @@ fun k m ->
    let%bind (src, dst) =
      trace_strong (simple_error "MAP_FIND: not map or bigmap") @@
      bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src, k) in
    ok @@ dst

  let map_find_opt : typer = typer_2 "MAP_FIND_OPT" @@ fun k m ->
    let%bind (src, dst) = bind_map_or (get_t_map , get_t_big_map) m in
    let%bind () = assert_type_expression_eq (src, k) in
    ok @@ t_option dst ()

  let map_iter : typer = typer_2 "MAP_ITER" @@ fun f m ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    let%bind () = assert_eq_1 res (t_unit ()) in
    ok @@ t_unit ()

  let map_map : typer = typer_2 "MAP_MAP" @@ fun f m ->
    let%bind (k, v) = get_t_map m in
    let%bind (arg , res) = get_t_function f in
    let%bind () = assert_eq_1 arg (t_pair k v ()) in
    ok @@ t_map k res ()

  let size = typer_1 "SIZE" @@ fun t ->
    let%bind () =
      Assert.assert_true @@
      (is_t_map t || is_t_list t || is_t_string t || is_t_bytes t || is_t_set t ) in
    ok @@ t_nat ()

  let slice = typer_3 "SLICE" @@ fun i j s ->
    let%bind () = assert_eq_1 i (t_nat ()) in
    let%bind () = assert_eq_1 j (t_nat ()) in
    if eq_1 s (t_string ())
    then ok @@ t_string ()
    else if eq_1 s (t_bytes ())
    then ok @@ t_bytes ()
    else fail @@ Operator_errors.typeclass_error "Computing slice with wrong types" "slice"
                   [
                     [t_nat();t_nat();t_string()] ;
                     [t_nat();t_nat();t_bytes()] ;
                   ]
                   [i ; j ; s] ()

  let failwith_ = typer_1_opt "FAILWITH" @@ fun t opt ->
    let%bind _ =
      if eq_1 t (t_string ())
      then ok ()
      else if eq_1 t (t_nat ())
      then ok ()
      else if eq_1 t (t_int ())
      then ok ()
      else
        fail @@ Operator_errors.typeclass_error "Failwith with disallowed type" "failwith"
          [
            [t_string()] ;
            [t_nat()] ;
            [t_int()] ;
          ]
          [t] () in
    let default = t_unit () in
    ok @@ Simple_utils.Option.unopt ~default opt

  let int : typer = typer_1 "INT" @@ fun t ->
    let%bind () = assert_t_nat t in
    ok @@ t_int ()

  let bytes_pack : typer = typer_1 "PACK" @@ fun _t ->
    ok @@ t_bytes ()

  let bytes_unpack = typer_1_opt "UNPACK" @@ fun input output_opt ->
    let%bind () = assert_t_bytes input in
    trace_option (simple_error "untyped UNPACK") @@
    output_opt

  let hash256 = typer_1 "SHA256" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let hash512 = typer_1 "SHA512" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let blake2b = typer_1 "BLAKE2b" @@ fun t ->
    let%bind () = assert_t_bytes t in
    ok @@ t_bytes ()

  let hash_key = typer_1 "HASH_KEY" @@ fun t ->
    let%bind () = assert_t_key t in
    ok @@ t_key_hash ()

  let check_signature = typer_3 "CHECK_SIGNATURE" @@ fun k s b ->
    let%bind () = assert_t_key k in
    let%bind () = assert_t_signature s in
    let%bind () = assert_t_bytes b in
    ok @@ t_bool ()

  let sender = constant' "SENDER" @@ t_address ()

  let source = constant' "SOURCE" @@ t_address ()

  let unit = constant' "UNIT" @@ t_unit ()

  let amount = constant' "AMOUNT" @@ t_mutez ()

  let balance = constant' "BALANCE" @@ t_mutez ()

  let chain_id = constant' "CHAIN_ID" @@ t_chain_id ()

  let address = typer_1 "ADDRESS" @@ fun contract ->
    let%bind () = assert_t_contract contract in
    ok @@ t_address ()

  let self_address = typer_0 "SELF_ADDRESS" @@ fun _ ->
    ok @@ t_address ()

  let self = typer_1_opt "SELF" @@ fun entrypoint_as_string tv_opt ->
    let%bind () = assert_t_string entrypoint_as_string in
    match tv_opt with
    | None -> simple_fail "untyped SELF"
    | Some t -> ok @@ t

  let implicit_account = typer_1 "IMPLICIT_ACCOUNT" @@ fun key_hash ->
    let%bind () = assert_t_key_hash key_hash in
    ok @@ t_contract (t_unit () ) ()

  let now = constant' "NOW" @@ t_timestamp ()

  let transaction = typer_3 "CALL" @@ fun param amount contract ->
    let%bind () = assert_t_mutez amount in
    let%bind contract_param = get_t_contract contract in
    let%bind () = assert_type_expression_eq (param , contract_param) in
    ok @@ t_operation ()

  let create_contract = typer_4 "CREATE_CONTRACT" @@ fun f kh_opt amount init_storage  ->
    let%bind (args , ret) = get_t_function f in
    let%bind (_,s) = get_t_pair args in
    let%bind (oplist,s') = get_t_pair ret in
    let%bind () = assert_t_mutez amount in
    let%bind (delegate) = get_t_option kh_opt in
    let%bind () = assert_type_expression_eq (s,s') in
    let%bind () = assert_type_expression_eq (s,init_storage) in
    let%bind () = assert_t_list_operation oplist in
    let%bind () = assert_t_key_hash delegate in
    ok @@ t_pair (t_operation ()) (t_address ()) ()

  let get_contract = typer_1_opt "CONTRACT" @@ fun addr_tv tv_opt ->
    if not (type_expression_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_contract expects an address, got %a" PP.type_expression addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_contract needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_contract has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let get_contract_opt = typer_1_opt "CONTRACT OPT" @@ fun addr_tv tv_opt ->
    if not (type_expression_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_contract_opt expects an address, got %a" PP.type_expression addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_contract_opt needs a type annotation") tv_opt in
    let%bind tv =
      trace_strong (simple_error "get_entrypoint_opt has a not-option annotation") @@
      get_t_option tv in
    let%bind tv' =
      trace_strong (simple_error "get_entrypoint_opt has a not-option(contract) annotation") @@
      get_t_contract tv in
    ok @@ t_option (t_contract tv' ()) ()

  let get_entrypoint = typer_2_opt "CONTRACT_ENTRYPOINT" @@ fun entry_tv addr_tv tv_opt ->
    if not (type_expression_eq (entry_tv, t_string ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint expects a string entrypoint label for first argument, got %a" PP.type_expression entry_tv)
    else
    if not (type_expression_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint expects an address for second argument, got %a" PP.type_expression addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_entrypoint needs a type annotation") tv_opt in
    let%bind tv' =
      trace_strong (simple_error "get_entrypoint has a not-contract annotation") @@
      get_t_contract tv in
    ok @@ t_contract tv' ()

  let get_entrypoint_opt = typer_2_opt "CONTRACT_ENTRYPOINT_OPT" @@ fun entry_tv addr_tv tv_opt ->
    if not (type_expression_eq (entry_tv, t_string ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint_opt expects a string entrypoint label for first argument, got %a" PP.type_expression entry_tv)
    else
    if not (type_expression_eq (addr_tv, t_address ()))
    then fail @@ simple_error (Format.asprintf "get_entrypoint_opt expects an address for second argument, got %a" PP.type_expression addr_tv)
    else
    let%bind tv =
      trace_option (simple_error "get_entrypoint_opt needs a type annotation") tv_opt in
    let%bind tv =
      trace_strong (simple_error "get_entrypoint_opt has a not-option annotation") @@
      get_t_option tv in
    let%bind tv' =
      trace_strong (simple_error "get_entrypoint_opt has a not-option(contract) annotation") @@
      get_t_contract tv in
    ok @@ t_option (t_contract tv' ())()

  let set_delegate = typer_1 "SET_DELEGATE" @@ fun delegate_opt ->
    let%bind () = assert_eq_1 delegate_opt (t_option (t_key_hash ()) ()) in
    ok @@ t_operation ()

  let abs = typer_1 "ABS" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_nat ()

  let is_nat = typer_1 "ISNAT" @@ fun t ->
    let%bind () = assert_t_int t in
    ok @@ t_option (t_nat ()) ()

  let neg = typer_1 "NEG" @@ fun t ->
    let%bind () = Assert.assert_true (eq_1 t (t_nat ()) || eq_1 t (t_int ())) in
    ok @@ t_int ()

  let assertion = typer_1 "ASSERT" @@ fun a ->
    if eq_1 a (t_bool ())
    then ok @@ t_unit ()
    else fail @@ Operator_errors.type_error "Asserting a non-bool" a (t_bool ()) ()

  let times = typer_2 "TIMES" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if (eq_1 a (t_nat ()) && eq_1 b (t_mutez ())) || (eq_1 b (t_nat ()) && eq_1 a (t_mutez ()))
    then ok @@ t_mutez () else
      fail @@ Operator_errors.typeclass_error "Multiplying with wrong types" "multiply"
                [
                  [t_nat();t_nat()] ;
                  [t_int();t_int()] ;
                  [t_nat();t_mutez()] ;
                  [t_mutez();t_nat()] ;
                ]
                [a; b] ()

  let ediv = typer_2 "EDIV" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_option (t_pair (t_nat ()) (t_nat ()) ()) () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_option (t_pair (t_int ()) (t_nat ()) ()) () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
    then ok @@ t_option (t_pair (t_nat ()) (t_mutez ()) ()) () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
    then ok @@ t_option (t_pair (t_mutez ()) (t_mutez ()) ()) () else
      fail @@ Operator_errors.typeclass_error "Dividing with wrong types" "divide"
                [
                  [t_nat();t_nat()] ;
                  [t_int();t_int()] ;
                  [t_mutez();t_nat()] ;
                  [t_mutez();t_mutez()] ;
                ]
                [a; b] ()

  let div = typer_2 "DIV" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_nat ())
    then ok @@ t_mutez () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
    then ok @@ t_nat () else
      fail @@ Operator_errors.typeclass_error "Dividing with wrong types" "divide"
                [
                  [t_nat();t_nat()] ;
                  [t_int();t_int()] ;
                  [t_mutez();t_nat()] ;
                  [t_mutez();t_mutez()] ;
                ]
                [a; b] ()

  let mod_ = typer_2 "MOD" @@ fun a b ->
    if (eq_1 a (t_nat ()) || eq_1 a (t_int ())) && (eq_1 b (t_nat ()) || eq_1 b (t_int ()))
    then ok @@ t_nat () else
    if eq_1 a (t_mutez ()) && eq_1 b (t_mutez ())
    then ok @@ t_mutez () else
      fail @@ Operator_errors.typeclass_error "Computing modulo with wrong types" "modulo"
                [
                  [t_nat();t_nat()] ;
                  [t_nat();t_int()] ;
                  [t_int();t_nat()] ;
                  [t_int();t_int()] ;
                  [t_mutez();t_mutez()] ;
                ]
                [a; b] ()

  let add = typer_2 "ADD" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat () else
    if eq_2 (a , b) (t_int ())
    then ok @@ t_int () else
    if eq_2 (a , b) (t_mutez ())
    then ok @@ t_mutez () else
    if (eq_1 a (t_nat ()) && eq_1 b (t_int ())) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
    then ok @@ t_int () else
    if (eq_1 a (t_timestamp ()) && eq_1 b (t_int ())) || (eq_1 b (t_timestamp ()) && eq_1 a (t_int ()))
    then ok @@ t_timestamp () else
      fail @@ Operator_errors.typeclass_error "Adding modulo with wrong types" "add"
                [
                  [t_nat();t_nat()] ;
                  [t_int();t_int()] ;
                  [t_mutez();t_mutez()] ;
                  [t_nat();t_int()] ;
                  [t_int();t_nat()] ;
                  [t_timestamp();t_int()] ;
                  [t_int();t_timestamp()] ;
                ]
                [a; b] ()

  let set_mem = typer_2 "SET_MEM" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok @@ t_bool ()
    else fail @@ Operator_errors.type_error "Set.mem: elt and set don't match" elt key ()

  let set_add = typer_2 "SET_ADD" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok set
    else fail @@ Operator_errors.type_error "Set.add: elt and set don't match" elt key ()

  let set_remove = typer_2 "SET_REMOVE" @@ fun elt set ->
    let%bind key = get_t_set set in
    if eq_1 elt key
    then ok set
    else fail @@ Operator_errors.type_error "Set.remove: elt and set don't match" key elt ()

  let set_iter = typer_2 "SET_ITER" @@ fun body set ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_set set in
    if eq_1 key arg
    then ok (t_unit ())
    else fail @@ Operator_errors.type_error "bad set iter" key arg ()

  let list_empty = typer_0 "LIST_EMPTY" @@ fun tv_opt ->
    match tv_opt with
    | None -> simple_fail "untyped LIST_EMPTY"
    | Some t -> ok t

  let list_iter = typer_2 "LIST_ITER" @@ fun body lst ->
    let%bind (arg , res) = get_t_function body in
    let%bind () = Assert.assert_true (eq_1 res (t_unit ())) in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_unit ())
    else fail @@ Operator_errors.type_error "bad list iter" key arg ()

  let list_map = typer_2 "LIST_MAP" @@ fun body lst ->
    let%bind (arg , res) = get_t_function body in
    let%bind key = get_t_list lst in
    if eq_1 key arg
    then ok (t_list res ())
    else fail @@ Operator_errors.type_error "bad list map" key arg ()

  let list_fold = typer_3 "LIST_FOLD" @@ fun body lst init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind key = get_t_list lst in
    let msg = Format.asprintf "%a vs %a"
        PP.type_expression key
        PP.type_expression arg
    in
    trace (simple_error ("bad list fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" key cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  let set_fold = typer_3 "SET_FOLD" @@ fun body lst init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind key = get_t_set lst in
    let msg = Format.asprintf "%a vs %a"
        PP.type_expression key
        PP.type_expression arg
    in
    trace (simple_error ("bad set fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" key cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  let map_fold = typer_3 "MAP_FOLD" @@ fun body map init ->
    let%bind (arg , res) = get_t_function body in
    let%bind (prec , cur) = get_t_pair arg in
    let%bind (key , value) = get_t_map map in
    let msg = Format.asprintf "%a vs %a"
        PP.type_expression key
        PP.type_expression arg
    in
    trace (simple_error ("bad map fold:" ^ msg)) @@
    let%bind () = assert_eq_1 ~msg:"key cur" (t_pair key value ()) cur in
    let%bind () = assert_eq_1 ~msg:"prec res" prec res in
    let%bind () = assert_eq_1 ~msg:"res init" res init in
    ok res

  (** FOLD_WHILE is a fold operation that takes an initial value of a certain type
      and then iterates on it until a condition is reached. The auxillary function
      that does the fold returns either boolean true or boolean false to indicate
      whether the fold should continue or not. Necessarily then the initial value
      must match the input parameter of the auxillary function, and the auxillary
      should return type (bool * input) *)
  let fold_while = typer_2 "FOLD_WHILE" @@ fun body init ->
    let%bind (arg, result) = get_t_function body in
    let%bind () = assert_eq_1 arg init in
    let%bind () = assert_eq_1 (t_pair (t_bool ()) init ()) result
    in ok init

  (* Continue and Stop are just syntactic sugar for building a pair (bool * a') *)
  let continue = typer_1 "CONTINUE" @@ fun arg ->
    ok @@ t_pair (t_bool ()) arg ()

  let stop = typer_1 "STOP" @@ fun arg ->
    ok (t_pair (t_bool ()) arg ())

  let not_ = typer_1 "NOT" @@ fun elt ->
    if eq_1 elt (t_bool ())
    then ok @@ t_bool ()
    else if eq_1 elt (t_nat ()) || eq_1 elt (t_int ())
    then ok @@ t_int ()
    else fail @@ Operator_errors.type_error "bad parameter to not" elt (t_bool ()) ()

  let or_ = typer_2 "OR" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else fail @@ Operator_errors.typeclass_error "OR with wrong types" "or"
                   [
                     [t_bool();t_bool()] ;
                     [t_nat();t_nat()] ;
                   ]
                   [a; b] ()

  let xor = typer_2 "XOR" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else fail @@ Operator_errors.typeclass_error "XOR with wrong types" "xor"
                   [
                     [t_bool();t_bool()] ;
                     [t_nat();t_nat()] ;
                   ]
                   [a; b] ()

  let and_ = typer_2 "AND" @@ fun a b ->
    if eq_2 (a , b) (t_bool ())
    then ok @@ t_bool ()
    else if eq_2 (a , b) (t_nat ()) || (eq_1 b (t_nat ()) && eq_1 a (t_int ()))
    then ok @@ t_nat ()
    else fail @@ Operator_errors.typeclass_error "AND with wrong types" "and"
                   [
                     [t_bool();t_bool()] ;
                     [t_nat();t_nat()] ;
                     [t_int();t_nat()] ;
                   ]
                   [a; b] ()

  let lsl_ = typer_2 "LSL" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else fail @@ Operator_errors.typeclass_error "LSL with wrong types" "lsl"
                   [
                     [t_nat();t_nat()] ;
                   ]
                   [a; b] ()

  let lsr_ = typer_2 "LSR" @@ fun a b ->
    if eq_2 (a , b) (t_nat ())
    then ok @@ t_nat ()
    else fail @@ Operator_errors.typeclass_error "LSR with wrong types" "lsr"
                   [
                     [t_nat();t_nat()] ;
                   ]
                   [a; b] ()

  let concat = typer_2 "CONCAT" @@ fun a b ->
    if eq_2 (a , b) (t_string ())
    then ok @@ t_string ()
    else if eq_2 (a , b) (t_bytes ())
    then ok @@ t_bytes ()
    else fail @@ Operator_errors.typeclass_error "Concatenation with wrong types" "concat"
                   [
                     [t_string();t_string()] ;
                     [t_bytes();t_bytes()] ;
                   ]
                   [a; b] ()

  let cons = typer_2 "CONS" @@ fun hd tl ->
    let%bind elt = get_t_list tl in
    let%bind () = assert_eq_1 hd elt in
    ok tl

  let constant_typers c : typer result = match c with
    | C_INT                 -> ok @@ int ;
    | C_UNIT                -> ok @@ unit ;
    | C_NOW                 -> ok @@ now ;
    | C_IS_NAT              -> ok @@ is_nat ;
    | C_SOME                -> ok @@ some ;
    | C_NONE                -> ok @@ none ;
    | C_ASSERTION           -> ok @@ assertion ;
    | C_FAILWITH            -> ok @@ failwith_ ;
    (* LOOPS *)
    | C_FOLD_WHILE          -> ok @@ fold_while ;
    | C_FOLD_CONTINUE       -> ok @@ continue ;
    | C_FOLD_STOP           -> ok @@ stop ;
     (* MATH *)
    | C_NEG                 -> ok @@ neg ;
    | C_ABS                 -> ok @@ abs ;
    | C_ADD                 -> ok @@ add ;
    | C_SUB                 -> ok @@ sub ;
    | C_MUL                 -> ok @@ times ;
    | C_EDIV                -> ok @@ ediv ;
    | C_DIV                 -> ok @@ div ;
    | C_MOD                 -> ok @@ mod_ ;
    (* LOGIC *)
    | C_NOT                 -> ok @@ not_ ;
    | C_AND                 -> ok @@ and_ ;
    | C_OR                  -> ok @@ or_ ;
    | C_XOR                 -> ok @@ xor ;
    | C_LSL                 -> ok @@ lsl_;
    | C_LSR                 -> ok @@ lsr_;
    (* COMPARATOR *)
    | C_EQ                  -> ok @@ comparator "EQ" ;
    | C_NEQ                 -> ok @@ comparator "NEQ" ;
    | C_LT                  -> ok @@ comparator "LT" ;
    | C_GT                  -> ok @@ comparator "GT" ;
    | C_LE                  -> ok @@ comparator "LE" ;
    | C_GE                  -> ok @@ comparator "GE" ;
    (* BYTES / STRING *)
    | C_SIZE                -> ok @@ size ;
    | C_CONCAT              -> ok @@ concat ;
    | C_SLICE               -> ok @@ slice ;
    | C_BYTES_PACK          -> ok @@ bytes_pack ;
    | C_BYTES_UNPACK        -> ok @@ bytes_unpack ;
    (* SET  *)
    | C_SET_EMPTY           -> ok @@ set_empty ;
    | C_SET_ADD             -> ok @@ set_add ;
    | C_SET_REMOVE          -> ok @@ set_remove ;
    | C_SET_ITER            -> ok @@ set_iter ;
    | C_SET_FOLD            -> ok @@ set_fold ;
    | C_SET_MEM             -> ok @@ set_mem ;

    (* LIST *)
    | C_CONS                -> ok @@ cons ;
    | C_LIST_EMPTY          -> ok @@ list_empty ;
    | C_LIST_ITER           -> ok @@ list_iter ;
    | C_LIST_MAP            -> ok @@ list_map ;
    | C_LIST_FOLD           -> ok @@ list_fold ;
    (* MAP *)
    | C_MAP_EMPTY           -> ok @@ map_empty ;
    | C_BIG_MAP_EMPTY       -> ok @@ big_map_empty ;
    | C_MAP_ADD             -> ok @@ map_add ;
    | C_MAP_REMOVE          -> ok @@ map_remove ;
    | C_MAP_UPDATE          -> ok @@ map_update ;
    | C_MAP_ITER            -> ok @@ map_iter ;
    | C_MAP_MAP             -> ok @@ map_map ;
    | C_MAP_FOLD            -> ok @@ map_fold ;
    | C_MAP_MEM             -> ok @@ map_mem ;
    | C_MAP_FIND            -> ok @@ map_find ;
    | C_MAP_FIND_OPT        -> ok @@ map_find_opt ;
    (* BIG MAP *)
    (* CRYPTO *)
    | C_SHA256              -> ok @@ hash256 ;
    | C_SHA512              -> ok @@ hash512 ;
    | C_BLAKE2b             -> ok @@ blake2b ;
    | C_HASH_KEY            -> ok @@ hash_key ;
    | C_CHECK_SIGNATURE     -> ok @@ check_signature ;
    | C_CHAIN_ID            -> ok @@ chain_id ;
    (*BLOCKCHAIN *)
    | C_CONTRACT            -> ok @@ get_contract ;
    | C_CONTRACT_OPT        -> ok @@ get_contract_opt ;
    | C_CONTRACT_ENTRYPOINT -> ok @@ get_entrypoint ;
    | C_CONTRACT_ENTRYPOINT_OPT -> ok @@ get_entrypoint_opt ;
    | C_AMOUNT              -> ok @@ amount ;
    | C_BALANCE             -> ok @@ balance ;
    | C_CALL                -> ok @@ transaction ;
    | C_SENDER              -> ok @@ sender ;
    | C_SOURCE              -> ok @@ source ;
    | C_ADDRESS             -> ok @@ address ;
    | C_SELF                -> ok @@ self;
    | C_SELF_ADDRESS        -> ok @@ self_address;
    | C_IMPLICIT_ACCOUNT    -> ok @@ implicit_account;
    | C_SET_DELEGATE        -> ok @@ set_delegate ;
    | C_CREATE_CONTRACT     -> ok @@ create_contract ;
    | _                     -> simple_fail @@ Format.asprintf "Typer not implemented for consant %a" PP.constant c



end

module Compiler = struct
  (*
    Most constants pass through the Transpiler unchanged. So they need to be
    compiled down to Michelson. This is the last step.

    When compiling the constant, we need to provide its arity (through the type
    predicate, defined in `Helpers.Compiler`, and its michelson code.
    In the case of an n-ary constant, we assume that the stack has the form:
    `x1 :: x2 :: x3 ... :: xn :: _`.

    This step requires knowledge of Michelson. Knowledge of
    `Tezos_utils.Michelson` will help too, so that no Michelson has to actually
    be written by hand.
  *)

  include Helpers.Compiler
  open Tezos_utils.Michelson
  open Mini_c

  let get_operators c : predicate result =
  match c with
    | C_ADD             -> ok @@ simple_binary @@ prim I_ADD
    | C_SUB             -> ok @@ simple_binary @@ prim I_SUB
    | C_MUL             -> ok @@ simple_binary @@ prim I_MUL
    | C_EDIV            -> ok @@ simple_binary @@ prim I_EDIV
    | C_DIV             -> ok @@ simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "DIV by 0") ; i_car]
    | C_MOD             -> ok @@ simple_binary @@ seq [prim I_EDIV ; i_assert_some_msg (i_push_string "MOD by 0") ; i_cdr]
    | C_NEG             -> ok @@ simple_unary @@ prim I_NEG
    | C_OR              -> ok @@ simple_binary @@ prim I_OR
    | C_AND             -> ok @@ simple_binary @@ prim I_AND
    | C_XOR             -> ok @@ simple_binary @@ prim I_XOR
    | C_LSL             -> ok @@ simple_binary @@ prim I_LSL
    | C_LSR             -> ok @@ simple_binary @@ prim I_LSR
    | C_NOT             -> ok @@ simple_unary @@ prim I_NOT
    | C_PAIR            -> ok @@ simple_binary @@ prim I_PAIR
    | C_CAR             -> ok @@ simple_unary @@ prim I_CAR
    | C_CDR             -> ok @@ simple_unary @@ prim I_CDR
    | C_EQ              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_EQ]
    | C_NEQ             -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_NEQ]
    | C_LT              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_LT]
    | C_LE              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_LE]
    | C_GT              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_GT]
    | C_GE              -> ok @@ simple_binary @@ seq [prim I_COMPARE ; prim I_GE]
    | C_UPDATE          -> ok @@ simple_ternary @@ prim I_UPDATE
    | C_SOME            -> ok @@ simple_unary  @@ prim I_SOME
    | C_MAP_FIND        -> ok @@ simple_binary @@ seq [prim I_GET ; i_assert_some_msg (i_push_string "MAP FIND")]
    | C_MAP_MEM         -> ok @@ simple_binary @@ prim I_MEM
    | C_MAP_FIND_OPT    -> ok @@ simple_binary @@ prim I_GET
    | C_MAP_ADD         -> ok @@ simple_ternary @@ seq [dip (i_some) ; prim I_UPDATE]
    | C_MAP_UPDATE      -> ok @@ simple_ternary @@ prim I_UPDATE
    | C_FOLD_WHILE      -> ok @@ simple_binary @@ seq [i_swap ; (i_push (prim T_bool) (prim D_True));prim ~children:[seq [dip i_dup; i_exec; i_unpair]] I_LOOP ;i_swap ; i_drop]
    | C_FOLD_CONTINUE   -> ok @@ simple_unary @@ seq [(i_push (prim T_bool) (prim D_True)); i_pair]
    | C_FOLD_STOP       -> ok @@ simple_unary @@ seq [(i_push (prim T_bool) (prim D_False)); i_pair]
    | C_SIZE            -> ok @@ simple_unary @@ prim I_SIZE
    | C_FAILWITH        -> ok @@ simple_unary @@ prim I_FAILWITH
    | C_ASSERT_INFERRED -> ok @@ simple_binary @@ i_if (seq [i_failwith]) (seq [i_drop ; i_push_unit])
    | C_ASSERTION       -> ok @@ simple_unary @@ i_if (seq [i_push_unit]) (seq [i_push_string "failed assertion" ; i_failwith])
    | C_INT             -> ok @@ simple_unary @@ prim I_INT
    | C_ABS             -> ok @@ simple_unary @@ prim I_ABS
    | C_IS_NAT          -> ok @@ simple_unary @@ prim I_ISNAT
    | C_CONS            -> ok @@ simple_binary @@ prim I_CONS
    | C_UNIT            -> ok @@ simple_constant @@ prim I_UNIT
    | C_BALANCE         -> ok @@ simple_constant @@ prim I_BALANCE
    | C_AMOUNT          -> ok @@ simple_constant @@ prim I_AMOUNT
    | C_ADDRESS         -> ok @@ simple_unary @@ prim I_ADDRESS
    | C_SELF_ADDRESS    -> ok @@ simple_constant @@ seq [prim I_SELF; prim I_ADDRESS]
    | C_IMPLICIT_ACCOUNT -> ok @@ simple_unary @@ prim I_IMPLICIT_ACCOUNT
    | C_SET_DELEGATE    -> ok @@ simple_unary @@ prim I_SET_DELEGATE
    | C_NOW             -> ok @@ simple_constant @@ prim I_NOW
    | C_CALL            -> ok @@ simple_ternary @@ prim I_TRANSFER_TOKENS
    | C_SOURCE          -> ok @@ simple_constant @@ prim I_SOURCE
    | C_SENDER          -> ok @@ simple_constant @@ prim I_SENDER
    | C_SET_MEM         -> ok @@ simple_binary @@ prim I_MEM
    | C_SET_ADD         -> ok @@ simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_True)) ; prim I_UPDATE]
    | C_SET_REMOVE      -> ok @@ simple_binary @@ seq [dip (i_push (prim T_bool) (prim D_False)) ; prim I_UPDATE]
    | C_SLICE           -> ok @@ simple_ternary @@ seq [prim I_SLICE ; i_assert_some_msg (i_push_string "SLICE")]
    | C_SHA256          -> ok @@ simple_unary @@ prim I_SHA256
    | C_SHA512          -> ok @@ simple_unary @@ prim I_SHA512
    | C_BLAKE2b         -> ok @@ simple_unary @@ prim I_BLAKE2B
    | C_CHECK_SIGNATURE -> ok @@ simple_ternary @@ prim I_CHECK_SIGNATURE
    | C_HASH_KEY        -> ok @@ simple_unary @@ prim I_HASH_KEY
    | C_BYTES_PACK      -> ok @@ simple_unary @@ prim I_PACK
    | C_CONCAT          -> ok @@ simple_binary @@ prim I_CONCAT
    | C_CHAIN_ID        -> ok @@ simple_constant @@ prim I_CHAIN_ID
    | _                 -> simple_fail @@ Format.asprintf "operator not implemented for %a" Stage_common.PP.constant c


  (*
    Some complex operators will need to be added in compiler/compiler_program.
    All operators whose compilations involve a type are found there.
  *)

end
