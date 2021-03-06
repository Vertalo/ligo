(* type file_location = { *)
(*   filename : string ; *)
(*   start_line : int ; *)
(*   start_column : int ; *)
(*   end_line : int ; *)
(*   end_column : int ; *)
(* } *)

type virtual_location = string

type t =
  | File of Region.t (* file_location *)
  | Virtual of virtual_location

let make (start_pos:Lexing.position) (end_pos:Lexing.position) : t =
  (* TODO: give correct unicode offsets (the random number is here so
     that searching for wrong souce locations appearing in messages
     will quickly lead here *)
  File (Region.make
          ~start:(Pos.make ~byte:start_pos ~point_num:(-1897000) ~point_bol:(-1897000))
          ~stop:(Pos.make ~byte:end_pos ~point_num:(-1897000) ~point_bol:(-1897000)))

let virtual_location s = Virtual s
let dummy = virtual_location "dummy"

type 'a wrap = {
  wrap_content : 'a ;
  location : t ;
}

let wrap ~loc wrap_content = { wrap_content ; location = loc }
let unwrap { wrap_content ; _ } = wrap_content
let map f x = { x with wrap_content = f x.wrap_content }
let pp_wrap f ppf { wrap_content ; _ } = Format.fprintf ppf "%a" f wrap_content

let lift_region : 'a Region.reg -> 'a wrap = fun x ->
  wrap ~loc:(File x.region) x.value
