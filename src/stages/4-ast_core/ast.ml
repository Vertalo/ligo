[@@@warning "-30"]

open Types_utils
include Stage_common.Enums (*@ follow ../common/enums.ml *)

type string_option = string option
  [@@deriving yojson]

type attribute = {
  inline: bool ;
}
[@@deriving yojson] 

let location_of_yojson loc = Location.of_yojson loc
let location_to_yojson loc = Location.to_yojson loc

type program_loc = declaration location_wrap
  [@@deriving yojson]
and program = program_loc list
  [@@deriving yojson]

and binder = { 
  var : expression_variable ;
  ty : type_expression ;
  }
  [@@deriving yojson]

and declaration_type = {
    type_binder : type_variable ;
    type_expr : type_expression ;
  }
  [@@deriving yojson]

and declaration_constant = {
    binder : binder;
    attr : attribute ;
    expr : expression ;
  }
  [@@deriving yojson]
and declaration =
  | Declaration_type of declaration_type
  (* A Declaration_constant is described by
   *   a name
   *   an optional type annotation
   *   a boolean indicating whether it should be inlined
   *   an expression *)
  | Declaration_constant of declaration_constant
  [@@deriving yojson]

(* | Macro_declaration of macro_declaration *)

and field_label_map = row_element label_map
  [@@deriving yojson]
and type_expression_list = type_expression list
  [@@deriving yojson]

and content_type_constant = {
    type_constant : type_constant ;
    arguments : type_expression_list ;
  }
  [@@deriving yojson]
and type_content =
  | T_sum of field_label_map
  | T_record of field_label_map
  | T_arrow of arrow
  | T_variable of type_variable
  (* TODO: remove this when we remove the old typer *)
  | T_wildcard
  | T_constant of content_type_constant
  [@@deriving yojson]

and arrow = {
    type1: type_expression ;
    type2: type_expression ;
  }
  [@@deriving yojson]
and row_element = {
    associated_type : type_expression ;
    michelson_annotation : string_option ;
    decl_pos : int ;
  }
  [@@deriving yojson]

and type_expression = {
  type_content  : type_content ;
  sugar    : sugar_type_expression_option ;
  location : location ;
  }
  [@@deriving yojson]

and expression = {
  content  : expression_content ;
  sugar    : sugar_expression_option ;
  location : location ;
  }
  [@@deriving yojson]

and expression_label_map = expression label_map
and expression_content =
  | E_literal of literal
  | E_constant of constant
  | E_variable of expression_variable
  | E_application of application
  | E_lambda of lambda
  | E_recursive of recursive
  | E_let_in of let_in
  | E_raw_code of raw_code
  | E_constructor of constructor
  | E_matching of matching
  | E_record of expression_label_map
  | E_record_accessor of record_accessor
  | E_record_update   of record_update
  | E_ascription of ascription
  [@@deriving yojson]

and expression_list = expression list
  [@@deriving yojson]

and constant = {
    cons_name: constant' ;
    arguments: expression_list ;
  }
  [@@deriving yojson]

and application = {
    lamb: expression ;
    args: expression ;
  }
  [@@deriving yojson]


and lambda = {
    binder: binder ;
    result: expression ;
  }
  [@@deriving yojson]

and recursive = {
    fun_name : expression_variable ;
    fun_type : type_expression ;
    lambda : lambda ;
  }
  [@@deriving yojson]
 
and let_in = {
    let_binder: binder ;
    rhs: expression ;
    let_result: expression ;
    inline: bool ;
  }
  [@@deriving yojson]

and raw_code = { 
  language : string ;
  code : expression ;
  }
  [@@deriving yojson]

and constructor = {
    constructor: label ;
    element: expression ;
  }
  [@@deriving yojson]

and record_accessor = {
    record: expression ;
    path: label ;
  }
  [@@deriving yojson]
and record_update = {
    record: expression ;
    path: label ;
    update: expression ;
  }
  [@@deriving yojson]
and match_cons = {
    hd : expression_variable ;
    tl : expression_variable ;
    body : expression ;
  }
  [@@deriving yojson]
and match_list = {
    match_nil  : expression ;
    match_cons : match_cons ;
  }
  [@@deriving yojson]
and match_some = {
    opt : expression_variable ;
    body : expression ;
  }
  [@@deriving yojson]
and match_option = {
    match_none : expression ;
    match_some : match_some ;
  }
  [@@deriving yojson]
and match_variant = {
    constructor : label ;
    proj : expression_variable ;
    body : expression ;
  }
  [@@deriving yojson]

and match_variant_list = match_variant list
  [@@deriving yojson]
and matching_expr =
  | Match_list of match_list
  | Match_option of match_option
  | Match_variant of match_variant_list
  [@@deriving yojson]

and matching = {
    matchee: expression ;
    cases: matching_expr ;
  }
  [@@deriving yojson]

and ascription = {
    anno_expr: expression ;
    type_annotation: type_expression ;
  }
  [@@deriving yojson]
