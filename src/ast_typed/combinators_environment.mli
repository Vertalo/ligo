open Types

val make_a_e_empty : expression -> type_value -> annotated_expression

val e_a_empty_unit : annotated_expression
val e_a_empty_int : int -> annotated_expression
val e_a_empty_nat : int -> annotated_expression
val e_a_empty_tez : int -> annotated_expression
val e_a_empty_bool : bool -> annotated_expression
val e_a_empty_string : string -> annotated_expression
val e_a_empty_address : string -> annotated_expression
val e_a_empty_pair : annotated_expression -> annotated_expression -> annotated_expression
val e_a_empty_some : annotated_expression -> annotated_expression
val e_a_empty_none : type_value -> annotated_expression
val e_a_empty_tuple : annotated_expression list -> annotated_expression
val e_a_empty_record : ae_map -> annotated_expression
val e_a_empty_map : (annotated_expression * annotated_expression ) list -> type_value -> type_value -> annotated_expression
val e_a_empty_list : annotated_expression list -> type_value -> annotated_expression
val ez_e_a_empty_record : ( name * annotated_expression ) list -> annotated_expression
val e_a_empty_lambda : lambda -> annotated_expression

val env_sum_type : ?env:full_environment -> ?name:name -> (name * type_value) list ->  full_environment