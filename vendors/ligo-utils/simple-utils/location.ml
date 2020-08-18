(* type file_location = { *)
(*   filename : string ; *)
(*   start_line : int ; *)
(*   start_column : int ; *)
(*   end_line : int ; *)
(*   end_column : int ; *)
(* } *)

type virtual_location = string
  [@@deriving yojson]

type t =
  | File of Region.t (* file_location *)
  | Virtual of virtual_location
  [@@deriving yojson]

let pp = fun ppf t ->
  match t with
  | Virtual _s -> Format.fprintf ppf ""
  | File f -> Format.fprintf ppf "%s" (f#to_string `Point)

let pp_json = fun t ->
  match t with
  | Virtual s -> `Assoc ["virtual" , `String s]
  | File f ->
    `Assoc [
      ("file", `String f#file) ;
      ("from_row", `Int f#start#line) ;
      ("from_col", `Int (f#start#column `Point)) ;
      ("to_row", `Int f#stop#line) ;
      ("to_col", `Int (f#stop#column `Point)) ;
    ]

let compare a b = match a,b with
  | (File a, File b) -> Region.compare a b
  | (File _, Virtual _) -> -1
  | (Virtual _, File _) -> 1
  | (Virtual a, Virtual b) -> String.compare a b


let make (start_pos:Lexing.position) (end_pos:Lexing.position) : t =
  (* TODO: give correct unicode offsets (the random number is here so
     that searching for wrong souce locations appearing in messages
     will quickly lead here *)
  File (Region.make
          ~start:(Pos.make ~byte:start_pos ~point_num:(-1897000) ~point_bol:(-1897000))
          ~stop:(Pos.make ~byte:end_pos ~point_num:(-1897000) ~point_bol:(-1897000)))

let virtual_location s = Virtual s
let dummy = virtual_location "dummy"
let generated = virtual_location "generated"

type 'a wrap = {
  wrap_content : 'a ;
  location : t ;
}
  [@@deriving yojson]

let compare_wrap ~compare:compare_content { wrap_content = wca ; location = la } { wrap_content = wcb ; location = lb } =
  match compare_content wca wcb with
  | 0 -> compare la lb
  | c -> c

let compare_content ~compare:compare_content wa wb =
  compare_content wa.wrap_content wb.wrap_content

let equal_content ~equal:equal_content wa wb =
  equal_content wa.wrap_content wb.wrap_content

let wrap ?(loc = generated) wrap_content = { wrap_content ; location = loc }
let get_location x = x.location
let unwrap { wrap_content ; _ } = wrap_content
let map f x = { x with wrap_content = f x.wrap_content }
let pp_wrap f ppf { wrap_content ; _ } = Format.fprintf ppf "%a" f wrap_content

let lift_region : 'a Region.reg -> 'a wrap = fun x ->
  wrap ~loc:(File x.region) x.value
let lift : Region.region -> t = fun x -> File x
let pp_lift = fun ppf r -> pp ppf @@ lift r

let r_extract : 'a Region.reg -> t = fun x -> File x.region
let r_split : 'a Region.reg -> ('a * t) = fun x -> x.value , File x.region
