%{
(* START HEADER *)

    (*open AST*)

(* END HEADER *)
%}

(* Entry points *)

%start program
%type <unit> program (* AST.t *)

%%

(* RULES *)

(* Compounded constructs *)

paren(X): LPAREN X RPAREN { (*$1,$2,$3*) }
brace(X): LBRACE X RBRACE { (*$1,$2,$3*) }

(* Non-empty separated sequence of items *)

nsepseq(X,Sep):
  X                    {                        (*$1, []*) }
| X Sep nsepseq(X,Sep) { (*let h,t = $3 in $1, ($2,h)::t*) }

(* Possibly empty separated sequence of items *)

seq(X):
  nsepseq(X,SEMI) { (*$1*) }

(* Main *)

program:
  K_storage   type__ SEMI
  K_parameter type__ SEMI
  K_code instruction SEMI
  EOF {}
| K_parameter type__ SEMI
  K_storage   type__ SEMI
  K_code instruction SEMI
  EOF {}

type__:
  constant_type_constructor

| T_pair     option(Tannot) field_type field_type
| T_or       option(Tannot) variant_type variant_type
| T_option   option(Tannot) subtype

| T_list     option(Tannot) subtype
| T_set      option(Tannot) comparable_type
| T_contract option(Tannot) subtype
| T_lambda   option(Tannot) subtype subtype
| T_map      option(Tannot) comparable_type subtype
| T_big_map  option(Tannot) comparable_type subtype

| paren(type__)
| paren(constant_type_constructor Tannot {}) {}

field_type:
  constant_type_constructor

| T_pair     option(Fannot) field_type field_type
| T_or       option(Fannot) variant_type variant_type
| T_option   option(Fannot) subtype

| T_list     option(Fannot) subtype
| T_set      option(Fannot) comparable_type
| T_contract option(Fannot) subtype
| T_lambda   option(Fannot) subtype subtype
| T_map      option(Fannot) comparable_type subtype
| T_big_map  option(Fannot) comparable_type subtype

| paren(field_type) {}
| paren(constant_type_constructor Fannot {}) {}

%inline variant_type: field_type {}

subtype:
  constant_type_constructor

| T_pair     field_type field_type
| T_or       variant_type variant_type
| T_option   subtype

| T_list     subtype
| T_set      comparable_type
| T_contract subtype
| T_lambda   subtype subtype
| T_map      comparable_type subtype
| T_big_map  comparable_type subtype

| paren(type__)

comparable_type:
  T_int
| T_nat
| T_string
| T_bytes
| T_mutez
| T_bool
| T_key_hash
| T_timestamp {}

constant_type_constructor:
  comparable_type
| T_key
| T_unit
| T_signature
| T_operation
| T_address {}

%inline domain: type__ {}
%inline range: type__ {}

data:
  String
| Bytes
| Int
| brace(seq(D_Elt data data {})) (* Empty? *)
| D_False
| D_Left data
| D_None
| D_Pair data data
| D_Right data
| D_Some data
| D_True
| D_Unit {}

block:
  brace(seq(instruction)) {} (* Empty? *)

instruction:
  ABS
| ADD
| ADDRESS
| AMOUNT
| AND
| BALANCE
| BLAKE2B
| CAST
| CHECK_SIGNATURE
| COMPARE
| CONCAT
| CONS
| CONTRACT type__
| CREATE_ACCOUNT
| CREATE_CONTRACT block
| IMPLICIT_ACCOUNT
| DROP
| EDIV
| EMPTY_MAP comparable_type type__
| EMPTY_SET comparable_type
| EQ
| EXEC
| FAILWITH data
| GE
| GET
| GT
| HASH_KEY
| IF block block
| IF_CONS block block
| IF_LEFT block block
| IF_NONE block block
| IF_RIGHT block block
| INT
| ISNAT
| ITER block
| LAMBDA domain range block
| LE
| LEFT type__
| LOOP block
| LOOP_LEFT block
| LSL
| LSR
| LT
| MAP block
| MEM
| MUL
| NEG
| NEQ
| NIL type__
| NONE type__
| NOT
| NOW
| OR
| PACK
| PUSH type__ data
| RENAME
| RIGHT type__
| SELF
| SENDER
| SET_DELEGATE
| SHA256
| SHA512
| SIZE
| SLICE
| SOME
| SOURCE
| STEPS_TO_QUOTA
| SUB
| SWAP
| TRANSFER_TOKENS
| UNIT
| UNPACK
| UPDATE
| XOR
| macro
| block {}

macro:
  ASSERT
| ASSERT_CMPEQ
| ASSERT_CMPGE
| ASSERT_CMPGT
| ASSERT_CMPLE
| ASSERT_CMPLT
| ASSERT_CMPNEQ
| ASSERT_EQ
| ASSERT_GE
| ASSERT_GT
| ASSERT_LE
| ASSERT_LEFT
| ASSERT_LT
| ASSERT_NEQ
| ASSERT_NONE
| ASSERT_RIGHT
| ASSERT_SOME
| CMPEQ
| CMPGE
| CMPGT
| CMPLE
| CMPLT
| CMPNEQ
| FAIL
| IFCMPEQ
| IFCMPGE
| IFCMPGT
| IFCMPLE
| IFCMPLT
| IFCMPNEQ
| IFEQ
| IFGE
| IFGT
| IFLE
| IFLT
| IFNEQ
| IF_NONE
| IF_SOME

(* Non-constant macros *)

| PAIR
| UNPAIR
| DIP
| DUP
| CADR
| SET_CADR
| MAP_CADR {}
