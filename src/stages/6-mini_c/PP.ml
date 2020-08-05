[@@@coverage exclude_file]
open Simple_utils.PP_helpers
open Types
open Format

let list_sep_d x = list_sep x (tag " ,@ ")

let lr = fun ppf -> function `Left -> fprintf ppf "L" | `Right -> fprintf ppf "R"

let rec type_variable ppf : type_expression -> _ = fun te -> match te.type_content with
  | T_or(a, b) -> fprintf ppf "@[(%a) |@ (%a)@]" annotated a annotated b
  | T_pair(a, b) -> fprintf ppf "@[(%a) &@ (%a)@]" annotated a annotated b
  | T_base b -> type_constant ppf b
  | T_function(a, b) -> fprintf ppf "@[(%a) ->@ (%a)@]" type_variable a type_variable b
  | T_map(k, v) -> fprintf ppf "@[<4>map(%a -> %a)@]" type_variable k type_variable v
  | T_big_map(k, v) -> fprintf ppf "@[<9>big_map(%a -> %a)@]" type_variable k type_variable v
  | T_list(t) -> fprintf ppf "@[<5>list(%a)@]" type_variable t
  | T_set(t) -> fprintf ppf "@[<4>set(%a)@]" type_variable t
  | T_option(o) -> fprintf ppf "@[<7>option(%a)@]" type_variable o
  | T_contract(t) -> fprintf ppf "@[<9>contract(%a)@]" type_variable t

and annotated ppf : type_expression annotated -> _ = function
  | (Some ann, a) -> fprintf ppf "(%a %%%s)" type_variable a ann
  | (None, a) -> type_variable ppf a

and environment_element ppf ((n, tv) : environment_element) =
  Format.fprintf ppf "%a : %a" Var.pp n.wrap_content type_variable tv

and environment ppf (x:environment) =
  fprintf ppf "Env[%a]" (list_sep_d environment_element) x

and type_constant ppf (tb:type_base) : unit =
  let s = match tb with 
    | TB_unit      -> "unit"
    | TB_string    -> "string"
    | TB_bytes     -> "bytes"
    | TB_nat       -> "nat"
    | TB_int       -> "int"
    | TB_mutez     -> "mutez"
    | TB_bool      -> "bool"
    | TB_operation -> "operation"
    | TB_address   -> "address"
    | TB_key       -> "key"
    | TB_key_hash  -> "key_hash"
    | TB_signature -> "signature"
    | TB_timestamp -> "timestamp"
    | TB_chain_id  -> "chain_id"
    in
  fprintf ppf "%s" s

let rec value ppf : value -> unit = function
  | D_bool b -> fprintf ppf "%b" b
  | D_operation _ -> fprintf ppf "operation[...bytes]"
  | D_int n -> fprintf ppf "%a" Z.pp_print n
  | D_nat n -> fprintf ppf "+%a" Z.pp_print n
  | D_timestamp n -> fprintf ppf "+%a" Z.pp_print n
  | D_mutez n -> fprintf ppf "%amutez" Z.pp_print n
  | D_unit -> fprintf ppf "unit"
  | D_string s -> fprintf ppf "\"%s\"" s
  | D_bytes x ->
     fprintf ppf "0x%a" Hex.pp @@ Hex.of_bytes x
  | D_pair (a, b) -> fprintf ppf "(%a), (%a)" value a value b
  | D_left a -> fprintf ppf "L(%a)" value a
  | D_right b -> fprintf ppf "R(%a)" value b
  | D_none -> fprintf ppf "None"
  | D_some s -> fprintf ppf "Some (%a)" value s
  | D_map m -> fprintf ppf "Map[%a]" (list_sep_d value_assoc) m
  | D_big_map m -> fprintf ppf "Big_map[%a]" (list_sep_d value_assoc) m
  | D_list lst -> fprintf ppf "List[%a]" (list_sep_d value) lst
  | D_set lst -> fprintf ppf "Set[%a]" (list_sep_d value) lst

and type_expression_annotated ppf : type_expression annotated -> unit = fun (_, tv) ->
  type_expression ppf tv

and type_expression ppf : type_expression -> unit = fun te -> match te.type_content with
  | T_pair (a,b) -> fprintf ppf "pair %a %a" type_expression_annotated a type_expression_annotated b
  | T_or    (a,b) -> fprintf ppf "or %a %a" type_expression_annotated a type_expression_annotated b
  | T_function (a, b) -> fprintf ppf "lambda (%a) %a" type_expression a type_expression b
  | T_base tc -> fprintf ppf "%a" type_constant tc
  | T_map (k,v) -> fprintf ppf "Map (%a,%a)" type_expression k type_expression v
  | T_big_map (k,v) -> fprintf ppf "Big_map (%a,%a)" type_expression k type_expression v
  | T_list e -> fprintf ppf "List (%a)" type_expression e
  | T_set e -> fprintf ppf "Set (%a)" type_expression e
  | T_contract c -> fprintf ppf "Contract (%a)" type_expression c
  | T_option c -> fprintf ppf "Option (%a)" type_expression c 

and value_assoc ppf : (value * value) -> unit = fun (a, b) ->
  fprintf ppf "%a -> %a" value a value b

and expression ppf (e:expression) =
  fprintf ppf "%a" expression_content e.content

and expression_content ppf (e:expression_content) = match e with
  | E_closure x -> function_ ppf x
  | E_variable v -> fprintf ppf "%a" Var.pp v.wrap_content
  | E_application(a, b) -> fprintf ppf "@[(%a)@(%a)@]" expression a expression b

  | E_constant c -> fprintf ppf "@[%a@[<hv 1>(%a)@]@]" constant c.cons_name (list_sep_d expression) c.arguments
  | E_literal v -> fprintf ppf "@[L(%a)@]" value v
  | E_if_bool (c, a, b) ->
    fprintf ppf
      "@[match %a with@ @[<hv>| True ->@;<1 2>%a@ | False ->@;<1 2>%a@]@]"
      expression c expression a expression b
  | E_if_none (c, n, ((name, _) , s)) ->
    fprintf ppf
      "@[match %a with@ @[<hv>| None ->@;<1 2>%a@ | Some %a ->@;<1 2>%a@]@]"
      expression c expression n Var.pp name.wrap_content expression s
  | E_if_cons (c, n, (((hd_name, _) , (tl_name, _)) , cons)) ->
    fprintf ppf "@[%a ?? %a : (%a :: %a) -> %a@]"
      expression c expression n Var.pp hd_name.wrap_content Var.pp tl_name.wrap_content expression cons
  | E_if_left (c, ((name_l, _) , l), ((name_r, _) , r)) ->
      fprintf ppf
        "@[match %a with@ @[<hv>| Left %a ->@;<1 2>%a@ | Right %a ->@;<1 2>%a@]@]"
        expression c Var.pp name_l.wrap_content expression l Var.pp name_r.wrap_content expression r
  | E_let_in ((name , _) , inline, expr , body) ->
      fprintf ppf "@[let %a =@;<1 2>%a%a in@ %a@]" Var.pp name.wrap_content expression expr option_inline inline expression body
  | E_iterator (b , ((name , _) , body) , expr) ->
      fprintf ppf "@[for_%a %a of %a do ( %a )@]" constant b Var.pp name.wrap_content expression expr expression body
  | E_fold (((name , _) , body) , collection , initial) ->
      fprintf ppf "@[fold %a on %a with %a do ( %a )@]" expression collection expression initial Var.pp name.wrap_content expression body

  | E_record_update (r, path,update) ->
      fprintf ppf "@[{ %a@;<1 2>with@;<1 2>{ %a = %a } }@]" expression r (list_sep lr (const ".")) path expression update
  | E_raw_michelson code ->
      fprintf ppf "%s" code 

and expression_with_type : _ -> expression -> _  = fun ppf e ->
  fprintf ppf "%a : %a"
    expression_content e.content
    type_variable e.type_expression

and function_ ppf ({binder ; body}:anon_function) =
  fprintf ppf "@[fun %a ->@ (%a)@]"
    Var.pp binder.wrap_content
    expression body

and option_inline ppf inline = 
  if inline then 
    fprintf ppf "[@@inline]"
  else
    fprintf ppf ""

and declaration ppf ((n,i, e):assignment) = fprintf ppf "@[let %a =@;<1 2>%a%a@]" Var.pp n.wrap_content expression e option_inline i

and tl_statement ppf (ass, _) = declaration ppf ass

and program ppf (p:program) =
  fprintf ppf "@[<v>%a@]" (pp_print_list ~pp_sep:(tag "@ ") tl_statement) p

and constant ppf : constant' -> unit = function
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
  | C_FOLD                  -> fprintf ppf "FOLD"
  | C_FOLD_WHILE            -> fprintf ppf "FOLD_WHILE"
  | C_FOLD_CONTINUE         -> fprintf ppf "CONTINUE"
  | C_FOLD_STOP             -> fprintf ppf "STOP"
  | C_LOOP_LEFT             -> fprintf ppf "LOOP_LEFT"
  | C_LOOP_CONTINUE         -> fprintf ppf "LOOP_CONTINUE"
  | C_LOOP_STOP             -> fprintf ppf "LOOP_STOP"
  | C_ITER                  -> fprintf ppf "ITER"
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
  | C_LSL                   -> fprintf ppf "LSL"
  | C_LSR                   -> fprintf ppf "LSR"
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
  | C_CONTRACT_ENTRYPOINT   -> fprintf ppf "CONTRACT_ENTRYPOINT"
  | C_CONTRACT_OPT          -> fprintf ppf "CONTRACT OPT"
  | C_CONTRACT_ENTRYPOINT_OPT -> fprintf ppf "CONTRACT_ENTRYPOINT OPT"
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

let%expect_test _ =
  Format.printf "%a" value (D_bytes (Bytes.of_string "foo")) ;
  [%expect{| 0x666f6f |}]

let%expect_test _ =
  let pp = expression_content Format.std_formatter in
  let dummy_type = {type_content=T_base TB_unit;location=Location.generated} in
  let wrap e = { content = e ; type_expression = dummy_type ; location = Location.generated} in
  let y = Location.wrap ~loc:(Location.generated) (Var.of_name "y") in
  let z = Location.wrap ~loc:(Location.generated) (Var.of_name "z") in
  pp @@ E_closure { binder = y ; body = wrap (E_variable y) } ;
  [%expect{|
    fun y -> (y)
  |}] ;
  pp @@ E_closure { binder = z ; body = wrap (E_variable z) } ;
  [%expect{|
    fun z -> (z)
  |}]