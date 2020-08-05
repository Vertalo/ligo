open Test_helpers
open Main_errors

module Core = Typesystem.Core
open Ast_typed.Types
(* open Typesystem.Solver_types *)
open Trace
(* open Typer_common.Errors *)
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

let mk p_ctor_tag p_ctor_args = { tsrc = "unit test"; t = P_constant { p_ctor_tag ; p_ctor_args ; } ; }
(* A bunch of arbitrary types *)
let (int, unit, nat, string) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [])
(* An arbitrary two-argument type constructor. *)
let map (k,v) = mk C_map [k; v]
(* A bunch of type variables: *)
let (m,n,x,y,z) = let v name = Var.fresh ~name () in v "m", v "n", v "x", v "y", v "z"

let test''
    (name : string)
    (* Restriction function under test *)
    (restrict : c_constructor_simpl -> c_typeclass_simpl -> c_typeclass_simpl)
    (* New info: a variable assignment constraint: *)
    tv (_eq : string) c_tag tv_list
    (* Initial typeclass constraint: *)
    args (_in : string) tc
    (* Intermediate step (not tested): *)
    (_intermediate : type_value list option list)
    (* Expected restricted typeclass:: *)
    expected_args (_in : string) expected_tc =
  test name @@ fun () ->
  trace typer_tracer @@
  let info = { reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list } in
  let tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
  let expected =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
  (* TODO: use an error not an assert *)
  if Ast_typed.Compare_generic.c_typeclass_simpl (restrict info tc) expected = 0
  then let _ = assert false in ok()
  else ok ()

let tests1 restrict = [
  test'' "1" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_nat[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (**)        [ None               ;  Some []          ;  Some []             ; ]
    (* Expected restricted typeclass: *)
    [y;z]   "∈" [                      [      int ; int] ; [      int ; string] ; ]
  ;

  test'' "2" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_map[m;n]
    (* Initial typeclass constraint: *)
    [x;y]   "∈" [[int  ; unit] ; [map(nat,nat)   ; int] ; [map(nat,string)   ; int] ; ]
    (* Intermediate step (not tested): *)
    (**)        [ None         ;  Some [nat;nat]        ;  Some [nat;string]        ; ]
    (* Expected restricted typeclass constraint: *)
    [m;n;y] "∈" [                [nat ; nat      ; int] ; [nat ; string      ; int] ; ]
  ;

  test'' "3" restrict
    (* New info: a variable assignment constraint: *)
    y "=" C_nat[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (**)        [       None         ;        Some []    ;        Some []       ; ]
    (* Expected restricted typeclass: *)
    [x;z]   "∈" [                      [nat ;       int] ; [nat ;       string] ; ]
  ;    
]

let test'
    name
    (deduce_and_clean : c_typeclass_simpl -> (c_constructor_simpl list * c_typeclass_simpl, _) result)
    args (_in : string) tc
    (expected_inferred  : (type_variable * constant_tag * type_variable list) list)
    expected_args (_in : string) expected_tc =
  test name @@ fun () ->
  trace typer_tracer @@
  let input_tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
  let expected_tc =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
  let expected_inferred = List.map
      (fun (tv , c_tag , tv_list) -> {reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list})
      expected_inferred in
  let%bind (actual_inferred , actual_tc) = deduce_and_clean input_tc in
  assert (Ast_typed.Compare_generic.c_constructor_simpl_list actual_inferred expected_inferred = 0);
  assert (Ast_typed.Compare_generic.c_typeclass_simpl actual_tc expected_tc = 0);
  ok ()
let inferred v (_eq : string) c args = v, c, args
let tests2 deduce_and_clean = [
  test' "1" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [nat ; int] ; [nat ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int] ; [      string] ; ]
  ;

  test' "2" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;y;z] "∈" [ [nat ; int ; unit] ; [nat ; string ; unit] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ;
     inferred z "=" C_unit[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int       ] ; [      string       ] ; ]
  ;

  test' "3" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ] (* TODO: how to test this and not care about the actual m and n ? *)
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [    nat;unit  ; int] ; [    unit;nat  ; string] ; ]
  ;
]

let main = test_suite "Typer (from core AST)"
  @@ List.flatten
    [
      tests1 Typer_new.Heuristic_tc_fundep.restrict ;
      tests2 Typer_new.Heuristic_tc_fundep.deduce_and_clean ;
    ]
