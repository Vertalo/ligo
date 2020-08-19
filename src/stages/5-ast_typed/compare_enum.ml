open Stage_common.Enums

let type_constant_tag = function
  | TC_unit                      ->  1
  | TC_string                    ->  2
  | TC_bytes                     ->  3
  | TC_nat                       ->  4
  | TC_int                       ->  5
  | TC_mutez                     ->  6
  | TC_operation                 ->  7
  | TC_address                   ->  8
  | TC_key                       ->  9
  | TC_key_hash                  -> 10
  | TC_chain_id                  -> 11
  | TC_signature                 -> 12
  | TC_timestamp                 -> 13
  | TC_contract                  -> 14
  | TC_option                    -> 15
  | TC_list                      -> 16
  | TC_set                       -> 17
  | TC_map                       -> 18
  | TC_big_map                   -> 19
  | TC_map_or_big_map            -> 20
  | TC_michelson_pair            -> 21
  | TC_michelson_or              -> 22
  | TC_michelson_pair_right_comb -> 23
  | TC_michelson_pair_left_comb  -> 24
  | TC_michelson_or_right_comb   -> 25
  | TC_michelson_or_left_comb    -> 26

let type_constant a b = Int.compare (type_constant_tag a) (type_constant_tag b)

let constant'_tag = function
  | C_INT                     ->   1
  | C_UNIT                    ->   2
  | C_NIL                     ->   3
  | C_NOW                     ->   4
  | C_IS_NAT                  ->   5
  | C_SOME                    ->   6
  | C_NONE                    ->   7
  | C_ASSERTION               ->   8
  | C_ASSERT_INFERRED         ->   9
  | C_FAILWITH                ->  10
  | C_UPDATE                  ->  11
  (* Loops *)
  | C_ITER                    ->  12
  | C_FOLD_WHILE              ->  13
  | C_FOLD_CONTINUE           ->  14
  | C_FOLD_STOP               ->  15
  | C_LOOP_LEFT               ->  16
  | C_LOOP_CONTINUE           ->  17
  | C_LOOP_STOP               ->  18
  | C_FOLD                    ->  19
  (* MATH *)
  | C_NEG                     ->  20
  | C_ABS                     ->  21
  | C_ADD                     ->  22
  | C_SUB                     ->  23
  | C_MUL                     ->  24
  | C_EDIV                    ->  25
  | C_DIV                     ->  26
  | C_MOD                     ->  27
  (* LOGIC *)
  | C_NOT                     ->  28
  | C_AND                     ->  29
  | C_OR                      ->  30
  | C_XOR                     ->  31
  | C_LSL                     ->  32
  | C_LSR                     ->  33
  (* COMPARATOR *)
  | C_EQ                      ->  34
  | C_NEQ                     ->  35
  | C_LT                      ->  36
  | C_GT                      ->  37
  | C_LE                      ->  38
  | C_GE                      ->  39
  (* Bytes/ String *)
  | C_SIZE                    ->  40
  | C_CONCAT                  ->  41
  | C_SLICE                   ->  42
  | C_BYTES_PACK              ->  43
  | C_BYTES_UNPACK            ->  44
  | C_CONS                    ->  45
  (* Pair *)
  | C_PAIR                    ->  46
  | C_CAR                     ->  47
  | C_CDR                     ->  48
  | C_LEFT                    ->  49
  | C_RIGHT                   ->  50
  (* Set *)
  | C_SET_EMPTY               ->  51
  | C_SET_LITERAL             ->  52
  | C_SET_ADD                 ->  53
  | C_SET_REMOVE              ->  54
  | C_SET_ITER                ->  55
  | C_SET_FOLD                ->  56
  | C_SET_MEM                 ->  57
  (* List *)
  | C_LIST_EMPTY              ->  58
  | C_LIST_LITERAL            ->  59
  | C_LIST_ITER               ->  60
  | C_LIST_MAP                ->  61
  | C_LIST_FOLD               ->  62
  (* Maps *)
  | C_MAP                     ->  63
  | C_MAP_EMPTY               ->  64
  | C_MAP_LITERAL             ->  65
  | C_MAP_GET                 ->  66
  | C_MAP_GET_FORCE           ->  67
  | C_MAP_ADD                 ->  68
  | C_MAP_REMOVE              ->  69
  | C_MAP_UPDATE              ->  70
  | C_MAP_ITER                ->  71
  | C_MAP_MAP                 ->  72
  | C_MAP_FOLD                ->  73
  | C_MAP_MEM                 ->  74
  | C_MAP_FIND                ->  75
  | C_MAP_FIND_OPT            ->  76
  (* Big Maps *)
  | C_BIG_MAP                 ->  77
  | C_BIG_MAP_EMPTY           ->  78
  | C_BIG_MAP_LITERAL         ->  79
  (* Crypto *)
  | C_SHA256                  ->  80
  | C_SHA512                  ->  81
  | C_BLAKE2b                 ->  82
  | C_HASH                    ->  83
  | C_HASH_KEY                ->  84
  | C_CHECK_SIGNATURE         ->  85
  | C_CHAIN_ID                ->  86
  (* Blockchain *)
  | C_CALL                    ->  87
  | C_CONTRACT                ->  88
  | C_CONTRACT_OPT            ->  89
  | C_CONTRACT_ENTRYPOINT     ->  90
  | C_CONTRACT_ENTRYPOINT_OPT ->  91
  | C_AMOUNT                  ->  92
  | C_BALANCE                 ->  93
  | C_SOURCE                  ->  94
  | C_SENDER                  ->  95
  | C_ADDRESS                 ->  96
  | C_SELF                    ->  97
  | C_SELF_ADDRESS            ->  98
  | C_IMPLICIT_ACCOUNT        ->  99
  | C_SET_DELEGATE            -> 100
  | C_CREATE_CONTRACT         -> 101
  | C_CONVERT_TO_LEFT_COMB    -> 102
  | C_CONVERT_TO_RIGHT_COMB   -> 103
  | C_CONVERT_FROM_LEFT_COMB  -> 104
  | C_CONVERT_FROM_RIGHT_COMB -> 105

let constant' a b = Int.compare (constant'_tag a) (constant'_tag b)

let literal_tag = function
  | Literal_unit        ->  1
  | Literal_int _       ->  2
  | Literal_nat _       ->  3
  | Literal_timestamp _ ->  4
  | Literal_mutez _     ->  5
  | Literal_string _    ->  6
  | Literal_bytes _     ->  7
  | Literal_address _   ->  8
  | Literal_signature _ ->  9
  | Literal_key _       -> 10
  | Literal_key_hash _  -> 11
  | Literal_chain_id _  -> 12
  | Literal_operation _ -> 13

let literal a b = Int.compare (literal_tag a) (literal_tag b)
