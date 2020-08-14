include Enums

type location = Location.t
  [@@deriving yojson]
type 'a location_wrap = 'a Location.wrap 
[@@deriving yojson]

type attribute = {
  inline: bool ;
  }
  [@@deriving yojson]

type expression_
and expression_variable = expression_ Var.t Location.wrap
let expression_variable_to_yojson var = Location.wrap_to_yojson (Var.to_yojson) var
let expression_variable_of_yojson var = Location.wrap_of_yojson (Var.of_yojson) var
type type_
and type_variable = type_ Var.t
let type_variable_to_yojson var = Var.to_yojson var
let type_variable_of_yojson var = Var.of_yojson var

type label = Label of string [@@deriving yojson]
module LMap = Map.Make( struct type t = label let compare (Label a) (Label b) = String.compare a b end)
type 'a label_map = 'a LMap.t

let bindings_to_yojson f g xs = `List (List.map (fun (x,y) -> `List [f x; g y]) xs)
let label_map_to_yojson row_elem_to_yojson m =
  bindings_to_yojson label_to_yojson row_elem_to_yojson (LMap.bindings m)

let binding_of_json f g = function
  | `List [x;y] ->
     begin match f x, g y with
     | Ok x, Ok y -> Some (x,y)
     | _ -> None end
  | _ -> None

let err_bad_format =
  Utils.error_yojson_format
    "A label map, represented as an array [ [string , element] , ... ]."

let bindings_of_yojson f g = function
  | `List xs ->
     begin match Option.bind_map_list (binding_of_json f g) xs with
     | None -> err_bad_format
     | Some xs -> Ok xs end
  | _ -> err_bad_format

let label_map_of_yojson row_elem_of_yojson m =
  Stdlib.Result.map LMap.of_list (bindings_of_yojson label_of_yojson row_elem_of_yojson m)

type 'ty_expr row_element = {
  associated_type      : 'ty_expr ;
  michelson_annotation : string option ;
  decl_pos : int ;
  }
  [@@deriving yojson]

(* Type level types *)
type 'ty_exp sum = 'ty_exp row_element label_map
type 'ty_exp record = 'ty_exp row_element label_map
type 'ty_exp arrow = {
  type1: 'ty_exp ;
  type2: 'ty_exp ;
  }
  [@@deriving yojson]

type 'ty_exp ty_cons = (type_constant * 'ty_exp list)
  [@@deriving yojson]

(* Expression level types *)
type 'ty_exp binder = {
  var : expression_variable ;
  ty  : 'ty_exp ;
  }
  [@@deriving yojson]


type 'exp application = {
  lamb: 'exp ; 
  args: 'exp ;
  }
  [@@deriving yojson]

type 'exp constant =
  { cons_name: constant' (* this is in enum *)
  ; arguments: 'exp list }
  [@@deriving yojson]

type ('exp,'ty_exp) lambda = {
  binder: 'ty_exp binder; 
  result: 'exp 
  }
  [@@deriving yojson]

type ('exp, 'ty_exp) recursive = {
  fun_name :  expression_variable;
  fun_type : 'ty_exp;
  lambda : ('exp, 'ty_exp) lambda;
}
  [@@deriving yojson]

type 'exp raw_code = { 
  language : string ;
  code : 'exp ;
  }
  [@@deriving yojson]

type 'exp constructor = {constructor: label; element: 'exp}
  [@@deriving yojson]

type 'exp access =
  | Access_tuple of z
  | Access_record of string
  | Access_map of 'exp
  [@@deriving yojson]

type 'exp accessor = {record: 'exp; path: 'exp access list}
  [@@deriving yojson]
type 'exp update   = {record: 'exp; path: 'exp access list; update: 'exp}
  [@@deriving yojson]

type 'exp record_accessor = {record: 'exp; path: label}
  [@@deriving yojson]
type 'exp record_update   = {record: 'exp; path: label; update: 'exp}
  [@@deriving yojson]

type ('exp,'ty_exp) ascription = {anno_expr: 'exp; type_annotation: 'ty_exp}
  [@@deriving yojson]

type 'exp conditional = {
  condition   : 'exp ;
  then_clause : 'exp ;
  else_clause : 'exp ;
  }
  [@@deriving yojson]

and 'exp sequence = {
  expr1: 'exp ;
  expr2: 'exp ;
  }
  [@@deriving yojson]

and 'exp assign = {
  variable    : expression_variable ;
  access_path : 'exp access list ;
  expression  : 'exp ;
}
  [@@deriving yojson]

and 'exp for_ = {
  binder : expression_variable ;
  start  : 'exp ;
  final  : 'exp ;
  incr   : 'exp ;
  f_body : 'exp ;
}
  [@@deriving yojson]

and 'exp for_each = {
  fe_binder : expression_variable * expression_variable option ;
  collection : 'exp ;
  collection_type : collect_type ;
  fe_body : 'exp ;
}
  [@@deriving yojson]

and collect_type = 
 | Map
 | Set
 | List
  [@@deriving yojson]

and 'exp while_loop = {
  cond : 'exp ;
  body : 'exp ;
}
  [@@deriving yojson]
