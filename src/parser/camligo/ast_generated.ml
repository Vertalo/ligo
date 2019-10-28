(* AST *)
(* Language *)

  (* Singletons *)
type variable = string
and constructor = string
and int_ = int
and unit_ = unit
and string_ = string
and tz_ = int
and let_content =
  | Let_content of (variable Location.wrap * (param Location.wrap list) * (type_annotation_ Location.wrap option) * expression Location.wrap)
and type_annotation_ =
  | Type_annotation_ of (type_expression Location.wrap)
and program =
  | Program of ((statement Location.wrap list))
and statement =
  | Statement_variable_declaration of (let_content Location.wrap)
  | Statement_init_declaration of (let_content Location.wrap)
  | Statement_entry_declaration of (let_content Location.wrap)
  | Statement_type_declaration of (variable Location.wrap * type_expression Location.wrap)
and param =
  | Param_restricted_pattern of (restricted_pattern Location.wrap)
  | Param_implicit_named_param of (variable Location.wrap)
and p_record_element =
  | P_record_element of (variable Location.wrap * pattern Location.wrap)
and e_record_element =
  | E_record_element_record_explicit of (variable Location.wrap * expression_no_seq Location.wrap)
  | E_record_element_record_implicit of (variable Location.wrap)
and e_match_clause =
  | E_match_clause of (pattern Location.wrap * expression_no_match Location.wrap)
and t_record_element =
  | T_record_element of (variable Location.wrap * type_expression Location.wrap)

  (* Hierarchies *)
and pattern =
| P_record of ((p_record_element Location.wrap list))
| P_type_annotation of (pattern Location.wrap * restricted_type_expression Location.wrap)
| P_pair of (pattern Location.wrap * pattern Location.wrap)
| P_data_structure of (variable Location.wrap * (pattern Location.wrap list))
| P_application of (pattern Location.wrap * pattern Location.wrap)
| P_variable of (variable Location.wrap)
| P_constructor of (constructor Location.wrap)
| P_module_ident of ((constructor Location.wrap list) * variable Location.wrap)
| P_unit of (unit_ Location.wrap) | P_paren of (pattern Location.wrap) [@@deriving show]
and restricted_pattern =
| Pr_variable of (variable Location.wrap) | Pr_unit of (unit_ Location.wrap)
| Pr_restrict of (pattern Location.wrap) [@@deriving show]
and expression_main =
| Eh_tuple of ((expression_main Location.wrap list))
| Eh_type_annotation of (expression_main Location.wrap * restricted_type_expression Location.wrap)
| Eh_lt of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_le of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_gt of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_eq of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_neq of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_assign of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_cons of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_addition of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_substraction of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_multiplication of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_division of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_application of (expression_main Location.wrap * expression_main Location.wrap)
| Eh_data_structure of (variable Location.wrap * (expression_main Location.wrap list))
| Eh_name of (expression_main Location.wrap)
| Eh_variable of (variable Location.wrap)
| Eh_constructor of (constructor Location.wrap)
| Eh_module_ident of ((constructor Location.wrap list) * variable Location.wrap)
| Eh_accessor of (variable Location.wrap * (variable Location.wrap list))
| Eh_int of (int_ Location.wrap) | Eh_unit of (unit_ Location.wrap)
| Eh_string of (string_ Location.wrap) | Eh_tz of (tz_ Location.wrap)
| Eh_bottom of (expression Location.wrap) [@@deriving show]
and expression_no_seq =
| Es_let_in of (pattern Location.wrap * expression_no_seq Location.wrap * expression_no_seq Location.wrap)
| Es_fun of (pattern Location.wrap * expression_no_seq Location.wrap)
| Es_record of ((e_record_element Location.wrap list))
| Es_ifthenelse of (expression_no_seq Location.wrap * expression_no_seq Location.wrap * expression_no_seq Location.wrap)
| Es_ifthen of (expression_no_seq Location.wrap * expression_no_seq Location.wrap)
| Es_match of (expression_no_seq Location.wrap * (e_match_clause Location.wrap list))
| Es_main of (expression_main Location.wrap) [@@deriving show]
and expression_no_match =
| Em_let_in of (pattern Location.wrap * expression_no_match Location.wrap * expression_no_match Location.wrap)
| Em_fun of (pattern Location.wrap * expression_no_match Location.wrap)
| Em_record of ((e_record_element Location.wrap list))
| Em_ifthenelse of (expression_no_match Location.wrap * expression_no_match Location.wrap * expression_no_match Location.wrap)
| Em_ifthen of (expression_no_match Location.wrap * expression_no_match Location.wrap)
| Em_main of (expression_main Location.wrap) [@@deriving show]
and expression =
| E_sequence of ((expression Location.wrap list))
| E_let_in of (pattern Location.wrap * expression Location.wrap * expression Location.wrap)
| E_fun of (pattern Location.wrap * expression Location.wrap)
| E_record of ((e_record_element Location.wrap list))
| E_ifthenelse of (expression Location.wrap * expression Location.wrap * expression Location.wrap)
| E_ifthen of (expression Location.wrap * expression Location.wrap)
| E_match of (expression Location.wrap * (e_match_clause Location.wrap list))
| E_main of (expression_main Location.wrap) [@@deriving show]
and restricted_type_expression =
| Tr_application of (restricted_type_expression Location.wrap * restricted_type_expression Location.wrap)
| Tr_variable of (variable Location.wrap)
| Tr_paren of (type_expression Location.wrap) [@@deriving show]
and type_expression =
| T_record of ((t_record_element Location.wrap list))
| T_tuple of ((type_expression Location.wrap list))
| T_application of (type_expression Location.wrap * type_expression Location.wrap)
| T_variable of (variable Location.wrap)
| T_paren of (type_expression Location.wrap) [@@deriving show]
  (* Entry point *)
type entry_point = program Location.wrap


