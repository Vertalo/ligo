(* This module exports checks on scoping, called from the parser. *)

module Region = Simple_utils.Region

type t =
  Reserved_name       of AST.variable
| Duplicate_parameter of AST.variable
| Duplicate_variant   of AST.variable
| Non_linear_pattern  of AST.variable
| Duplicate_field     of AST.variable

type error = t

exception Error of t

val check_reserved_name : AST.variable -> unit
val check_pattern       : AST.pattern -> unit
val check_variants      : AST.variant Region.reg list -> unit
val check_parameters    : AST.param_decl list -> unit
val check_fields        : AST.field_decl Region.reg list -> unit
