open Types
open Stage_common.To_yojson

let rec type_expression {type_content=tc;location} =
  `Assoc [
    ("type_content", type_content tc);
    ("location", Location.to_yojson location);
  ]

and type_content = function
  | T_variable        t -> `List [ `String "t_variable"; type_variable_to_yojson t]
  | T_sum             t -> `List [ `String "t_sum";
                                   label_map row_element t.fields;
                                   attributes t.attributes]
  | T_record          t -> `List [ `String "t_record";
                                   label_map row_element t.fields;
                                   attributes t.attributes]
  | T_tuple           t -> `List [ `String "t_tuple";  list type_expression t]
  | T_arrow           t -> `List [ `String "t_arrow"; arrow type_expression t]
  | T_app             t -> `List [ `String "t_app"; t_app type_expression t]
  | T_module_accessor t -> `List [ `String "t_module_accessor"; module_access type_expression t]
  | T_singleton       t -> `List [ `String "t_singleton" ; literal t ]

and row_element {associated_type; attributes=attr; decl_pos} =
  `Assoc [
    ("associated_type", type_expression associated_type);
    ("attributes", attributes attr);
    ("decl_pos", `Int decl_pos);
  ]

let rec expression {expression_content=ec;location} =
  `Assoc [
    ("expression_content", expression_content ec);
    ("location", Location.to_yojson location);
  ]

and expression_content = function
  (* Base *)
  | E_literal     e -> `List [ `String "E_literal";     literal e ]
  | E_constant    e -> `List [ `String "E_constant";    constant expression e ]
  | E_variable    e -> `List [ `String "E_variable";    expression_variable_to_yojson e ]
  | E_application e -> `List [ `String "E_application"; application expression e ]
  | E_lambda      e -> `List [ `String "E_lambda";      lambda      expression type_expression e ]
  | E_recursive   e -> `List [ `String "E_recursive";   recursive   expression type_expression e ]
  | E_let_in      e -> `List [ `String "E_let_in";      let_in      e ]
  | E_type_in     e -> `List [ `String "E_type_in";     type_in   expression type_expression e ]
  | E_raw_code    e -> `List [ `String "E_raw_code";    raw_code    expression e ]
  (* Variant *)
  | E_constructor e -> `List [ `String "E_constructor"; constructor expression e ]
  | E_matching    e -> `List [ `String "E_matching";    matching e ]
  (* Record *)
  | E_record          e -> `List [ `String "E_record";      record      expression e ]
  | E_accessor        e -> `List [ `String "E_record_accessor"; accessor expression e ]
  | E_update          e -> `List [ `String "E_record_update";   update   expression e ]
  | E_ascription      e -> `List [ `String "E_ascription";      ascription expression type_expression e ]
  | E_module_accessor e -> `List [ `String "E_module_accessor"; module_access expression e]
  | E_cond            e -> `List [ `String "E_cond";            conditional expression e ]
  | E_sequence        e -> `List [ `String "E_sequence";        sequence    expression e ]
  | E_skip              -> `List [ `String "E_skip"; `Null ]
  | E_tuple           e -> `List [ `String "E_tuple"; list expression e ]
  (* Data Structures *)
  | E_map         e -> `List [ `String "E_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ]
  | E_big_map     e -> `List [ `String "E_big_map"; list (fun (k,v) -> `List [ expression k; expression v]) e ]
  | E_list        e -> `List [ `String "E_list"; list expression e]
  | E_set         e -> `List [ `String "E_set"; list expression e]

and let_in {let_binder;rhs;let_result;attributes=attr;mut} =
  `Assoc [
    ("let_binder", expression_variable_to_yojson @@ let_binder.var);
    ("rhs", expression rhs);
    ("let_result", expression let_result);
    ("attributes", attributes attr);
    ("mut", `Bool mut);
  ]

and matching {matchee; cases} =
  `Assoc [
    ("matchee", expression matchee);
    ("cases", matching_expr cases);
  ]

and matching_expr = function
  | Match_list    {match_nil;match_cons} -> `List [ `String "Match_list";
    `Assoc [
      ("match_nil", expression match_nil);
      ("match_cons", matching_content_cons match_cons);
    ]]
  | Match_option  {match_none;match_some} -> `List [ `String "Match_option";
    `Assoc [
      ("match_none", expression match_none);
      ("match_some", matching_content_some match_some);
    ]]
  | Match_variant m -> `List [ `String "Match_variant"; list matching_content_case m ]
  | Match_tuple   (lst,e) -> `List [ `String "Match_tuple";
  (*TODO*)
    `List [
      list (fun (b) -> `List [expression_variable_to_yojson b.var]) lst;
      expression e;
    ]]
  | Match_record (lst, e) -> `List [`String "Match_record";
  (*TODO*)
    `List [
      list (fun (l,b) -> `List [label l; expression_variable_to_yojson b.var]) lst;
      expression e;
    ]]
  | Match_variable (b,e) -> `List [`String "Match_varible";
    `List [expression_variable_to_yojson b.var; expression e];
    ]

and matching_content_cons (hd, tl, body) =
  `Assoc [
    ("hd", expression_variable_to_yojson hd);
    ("tl", expression_variable_to_yojson tl);
    ("body", expression body);
  ]

and matching_content_some (opt, body ) =
  `Assoc [
    ("opt", expression_variable_to_yojson opt);
    ("body", expression body);
  ]

and matching_content_case ((constructor, pattern), body) =
  `Assoc [
    ("constructor", label constructor);
    ("pattern", expression_variable_to_yojson pattern);
    ("body", expression body);
  ]

let declaration = function
  | Declaration_type     dt -> `List [ `String "Declaration_type"; declaration_type type_expression dt]
  | Declaration_constant dc -> `List [ `String "Declaration_constant"; declaration_constant expression type_expression dc]

let program = program declaration
