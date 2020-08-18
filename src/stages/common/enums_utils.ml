type ligo_string = Simple_utils.Ligo_string.t [@@deriving yojson]
type packed_internal_operation = Memory_proto_alpha.Protocol.Alpha_context.packed_internal_operation

type z = Z.t 
let [@warning "-32"] z_to_yojson x = `String (Z.to_string x)
let [@warning "-32"] z_of_yojson x =
  try match x with
      | `String s -> Ok (Z.of_string s)
      | _ -> Utils.error_yojson_format "JSON string"
  with
  | Invalid_argument _ ->
     Error "Invalid formatting.
            The Zarith library does not know how to handle this formatting."

(* Serialization of packed_internal_operation seems difficult and useless
   for now.
*)
let [@warning "-32"] packed_internal_operation_to_yojson _ = `String "packed_internal_operation"
let [@warning "-32"] packed_internal_operation_of_yojson _ = Error "packed_internal_operation"
