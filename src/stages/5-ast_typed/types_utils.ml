include Stage_common.Types

type 'a location_wrap = 'a Location.wrap
  [@@deriving yojson]

type expression_
and expression_variable = expression_ Var.t location_wrap
type type_
and type_variable = type_ Var.t
type z = Z.t


type 'a list_ne = 'a List.Ne.t
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation
type inline = bool

type 'a extra_info__comparable = {
  compare : 'a -> 'a -> int ;
}
