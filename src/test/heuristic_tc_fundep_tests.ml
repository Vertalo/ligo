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
(* A bunch of arbitrary types (they only need to be distrinct type constructors without arguments, feel free to replace the contents if/when some of these types move to the stdlib and aren't built-in anymore). *)
let (int, unit, nat, string, bytes, mutez) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [], mk C_bytes [], mk C_mutez [])
(* An arbitrary two-argument type constructor (this only needs to be a type constructor with two arguments, feel free to replace). *)
let map (k,v) = mk C_map [k; v]
(* A bunch of type variables: *)
let (m,n,o,p,x,y,z) = let v name = Var.fresh ~name () in v "m", v "n", v "o", v "p", v "x", v "y", v "z"

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
  let%bind e =
    trace typer_tracer @@
    let info = { reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list } in
    let tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
    let expected =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
    (* TODO: use an error not an assert *)
    (* Format.printf "\n\nActual: %a\n\n" Ast_typed.PP_generic.c_typeclass_simpl (restrict info tc);
     * Format.printf "\n\nExpected %a\n\n" Ast_typed.PP_generic.c_typeclass_simpl expected; *)
    if Ast_typed.Compare_generic.c_typeclass_simpl (restrict info tc) expected != 0 then ok @@ Some (test_internal __LOC__)
    else ok None
  in match e with None -> ok () | Some e -> fail e

let tests1 restrict = [
  (
  test'' "restrict1" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_nat[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (**)        [ None               ;  Some []          ;  Some []             ; ]
    (* Expected restricted typeclass: *)
    [y;z]   "∈" [                      [      int ; int] ; [      int ; string] ; ]
);

(  test'' "restrict2" restrict
    (* New info: a variable assignment constraint: *)
    x "=" C_map[m;n]
    (* Initial typeclass constraint: *)
    [x;y]   "∈" [[int  ; unit] ; [map(nat,nat)   ; int] ; [map(nat,string)   ; int] ; ]
    (* Intermediate step (not tested): *)
    (**)        [ None         ;  Some [nat;nat]        ;  Some [nat;string]        ; ]
    (* Expected restricted typeclass constraint: *)
    [m;n;y] "∈" [                [nat ; nat      ; int] ; [nat ; string      ; int] ; ]
)  ;

(  test'' "restrict3" restrict
    (* New info: a variable assignment constraint: *)
    y "=" C_int[]
    (* Initial typeclass constraint: *)
    [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
    (* Intermediate step (not tested): *)
    (**)        [       None         ;        Some []    ;        Some []       ; ]
    (* Expected restricted typeclass: *)
    [x;z]   "∈" [                      [nat ;       int] ; [nat ;       string] ; ]
)  ;    
]

let test'
    name
    (deduce_and_clean : c_typeclass_simpl -> (deduce_and_clean_result, _) result)
    args (_in : string) tc
    (expected_inferred  : (type_variable * constant_tag * type_variable list) list)
    expected_args (_in : string) expected_tc =
  test name @@ fun () ->
  let%bind e =
    trace typer_tracer @@
    let input_tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
    let expected_tc =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
    let expected_inferred = List.map
        (fun (tv , c_tag , tv_list) -> {reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list})
        expected_inferred in
    let%bind actual = deduce_and_clean input_tc in
    (match Heuristic_tc_fundep_tests_compare_cleaned.compare_and_check_vars_deduce_and_clean_result { deduced = expected_inferred ; cleaned = expected_tc } actual with
     | Ok _ -> ok @@ None
     | Error e -> ok @@ Some e)
  in
  match e with None -> ok () | Some e -> fail e

let inferred v (_eq : string) c args = v, c, args
let tests2 deduce_and_clean = [
  test' "deduce_and_clean split type constructor" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( bytes , mutez ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ]
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [      nat ; unit   ; int ] ; [      bytes ; mutez   ; string ] ; ]
  ;

  test' "deduce_and_clean recursive" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( bytes , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ; 
      inferred n "=" C_unit[]   ; ]
    (* Expected cleaned typeclass: *)
    [m;z]   "∈" [ [      nat ;          int ] ; [      bytes ;          string ] ; ]
  ;

  test' "deduce_and_clean remove recursive" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( nat , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ;
      inferred m "=" C_nat[]    ;
      inferred n "=" C_unit[]   ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [                     int ] ; [                     string ] ; ]
  ;

  test' "deduce_and_clean remove no-argument type constructor" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [nat ; int] ; [nat ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int] ; [      string] ; ]
  ;

  test' "deduce_and_clean remove two no-argument type constructors" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;y;z] "∈" [ [nat ; int ; unit] ; [nat ; string ; unit] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ;
     inferred z "=" C_unit[] ; ]
    (* Expected cleaned typeclass: *)
    [y]     "∈" [ [      int       ] ; [      string       ] ; ]
  ;

  test' "deduce_and_clean split type constructor (again)" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ]
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [    nat;unit  ; int] ; [    unit;nat  ; string] ; ]
  ;

  test' "deduce_and_clean two recursive" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;y;z]   "∈" [ [ map( nat , unit ) ; map( bytes , mutez ) ; int ] ; [ map( nat , unit ) ; map( bytes , unit ) ; string ] ; ]
    (* Expected inferred constraints: *)
    [ inferred x "=" C_map[m;n] ; 
      inferred m "=" C_nat[]    ;
      inferred n "=" C_unit[]   ;
      inferred y "=" C_map[o;p] ; 
      inferred o "=" C_bytes[]  ; 
    ]
    (* Expected cleaned typeclass: *)
    [p;z]     "∈" [ [                                  mutez   ; int ] ; [                                   unit   ; string ] ; ]
  ;
]

let main = test_suite "Typer: fundep heuriscic"
  @@ List.flatten
    [
      tests1 Typer_new.Heuristic_tc_fundep.restrict ;
      tests2 Typer_new.Heuristic_tc_fundep.deduce_and_clean ;
    ]
