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

module Compare_cleaned = struct
  type 'a tree = Leaf of 'a | Pair of 'a tree * 'a tree
    
  let (<?) ca cb =
    match ca with
    | Some t1 ->
      (match cb () with
       | Some t2 -> Some (Pair (t1, t2))
       | None -> None)
    | None -> None

  let list ~compare expected actual =
    List.fold_left2 (function
        | None -> fun _ _ -> None
        | Some t1 -> fun a b ->
          match compare a b with
          | None -> None
          | Some t2 -> Some (Pair (t1, t2)))
      expected actual

  type 'a cmp = 'a -> 'a -> type_variable tree option
  
  let rec c_equation : c_equation cmp = fun expected actual ->
    let { aval=a1; bval=a2 } = expected in
    let { aval=b1; bval=b2 } = actual in
    type_value a1 b1 <? fun () -> type_value a2 b2

  and tc_allowed : tc_allowed cmp = fun expected actual ->
    List.compare ~compare:type_value expected actual

  and typeclass : typeclass cmp = fun expected actual ->
    List.compare ~compare:tc_allowed expected actual

  and tc_args : tc_args cmp = fun expected actual ->
    List.compare ~compare:type_value expected actual

  and c_typeclass : c_typeclass cmp = fun expected actual ->
    let { tc_args=a1; typeclass=a2 } = expected in
    let { tc_args=b1; typeclass=b2 } = actual in
    tc_args a1 b1 <? fun () -> typeclass a2 b2

  and c_access_label : c_access_label cmp = fun expected actual ->
    let { c_access_label_tval=a1; accessor=a2; c_access_label_tvar=a3 } = expected in
    let { c_access_label_tval=b1; accessor=b2; c_access_label_tvar=b3 } = actual in
    type_value a1 b1 <? fun () -> Ast_typed.Compare_generic.label a2 b2 <? fun () -> type_variable a3 b3

  and type_constaint_ : type_constraint_ cmp = fun expected actual ->
    match expected, actual with
    | Ast_typed.Types.C_equation     a , Ast_typed.Types.C_equation     b -> c_equation a b
    | Ast_typed.Types.C_typeclass    a , Ast_typed.Types.C_typeclass    b -> c_typeclass a b
    | Ast_typed.Types.C_access_label a , Ast_typed.Types.C_access_label b -> c_access_label a b
    | (a, b) ->
      let different = Ast_typed.Compare_generic.type_constraint_ a b in
      assert (different != 0); different
  and type_constraint : type_constraint cmp = fun expected actual ->
    let { reason=_; c=a1 } = expected in
    let { reason=_; c=b1 } = actual in
    type_constaint_ a1 b1

  and p_constraints : p_constraints cmp = fun expected actual ->
    List.compare ~compare:type_constraint expected actual

  and type_variable : type_variable cmp = fun expected actual ->
    (* We compare type variables during a later pass. *)
    ignore (expected,actual); 0

  and p_forall : p_forall cmp = fun expected actual ->
    let { binder=_; constraints=a1; body=a2 } = expected in
    let { binder=_; constraints=b1; body=b2 } = actual in
    p_constraints a1 b1 <? fun () ->
      type_value a2 b2

  and p_ctor_args : p_ctor_args cmp = fun expected actual ->
    List.compare ~compare:type_value expected actual

  and p_constant : p_constant cmp = fun expected actual ->
    let { p_ctor_tag = a1; p_ctor_args = a2 } = expected in
    let { p_ctor_tag = b1; p_ctor_args = b2 } = actual in
    Ast_typed.Compare_generic.constant_tag a1 b1 <? fun () ->
      p_ctor_args a2 b2

  and p_apply : p_apply cmp = fun expected actual ->
    let { tf=a1; targ=a2 } = expected in
    let { tf=b1; targ=b2 } = actual in
    type_value a1 b1 <? fun () ->
      type_value a2 b2
  
  and p_row : p_row cmp = fun expected actual ->
    let { p_row_tag = a1; p_row_args = a2 } = expected in
    let { p_row_tag = b1; p_row_args = b2 } = actual in
    Ast_typed.Compare_generic.row_tag a1 b1 <? fun () ->
      LMap.compare type_value a2 b2

  and type_value_ : type_value_ cmp = fun expected actual ->
    match expected, actual with
    | (Ast_typed.Types.P_forall   a , Ast_typed.Types.P_forall   b) -> p_forall a b
    | (Ast_typed.Types.P_variable a , Ast_typed.Types.P_variable b) -> type_variable a b
    | (Ast_typed.Types.P_constant a , Ast_typed.Types.P_constant b) -> p_constant a b
    | (Ast_typed.Types.P_apply    a , Ast_typed.Types.P_apply    b) -> p_apply a b
    | (Ast_typed.Types.P_row      a , Ast_typed.Types.P_row      b) -> p_row a b
    | (a, b) ->
      let different = Ast_typed.Compare_generic.type_value_ a b in
      assert (different != 0); different

  and type_value : type_value cmp = fun expected actual ->
    let { tsrc=_; t=a1 } = expected in
    let { tsrc=_; t=b1 } = actual in
    type_value_ a1 b1

  and c_typeclass_simpl : c_typeclass_simpl cmp = fun expected actual ->
    let { reason_typeclass_simpl=_; tc=a1; args=a2 } = expected in
    let { reason_typeclass_simpl=_; tc=b1; args=b2 } = actual in
    typeclass a1 b1 <? fun () ->
      Ast_typed.Compare_generic.type_variable_list a2 b2
end

let test'
    name
    (deduce_and_clean : c_typeclass_simpl -> (c_constructor_simpl list * c_typeclass_simpl, _) result)
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
    let%bind (actual_inferred , actual_tc) = deduce_and_clean input_tc in
    if Ast_typed.Compare_generic.c_constructor_simpl_list actual_inferred expected_inferred != 0
    then ok @@ Some (test_internal_msg __LOC__
                     @@ Format.asprintf "expected %a but got %a"
                       Ast_typed.PP_generic.c_constructor_simpl_list expected_inferred
                       Ast_typed.PP_generic.c_constructor_simpl_list actual_inferred)
    else if Ast_typed.Compare_generic.c_typeclass_simpl actual_tc expected_tc = 0
    then ok @@ Some (test_internal_msg __LOC__
                     @@ Format.asprintf "expected %a but got %a"
                       Ast_typed.PP_generic.c_typeclass_simpl actual_tc
                       Ast_typed.PP_generic.c_typeclass_simpl expected_tc)
    else ok @@ None
  in
  match e with None -> ok () | Some e -> fail e

let inferred v (_eq : string) c args = v, c, args
let tests2 deduce_and_clean = [
  (* test' "deduce_and_clean1" deduce_and_clean
   *   (\* Input restricted typeclass: *\)
   *   [x;z]   "∈" [ [ map( nat , unit ) ; int ] ; [ map( bytes , mutez ) ; string ] ; ]
   *   (\* Expected inferred constraints: *\)
   *   [inferred x "=" C_map[m;n] ; ]
   *   (\* Expected cleaned typeclass: *\)
   *   [z]     "∈" [ [      int] ; [      string] ; ]
   * ; *)

  test' "deduce_and_clean2" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [nat ; int] ; [nat ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int] ; [      string] ; ]
  ;

  test' "deduce_and_clean3" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;y;z] "∈" [ [nat ; int ; unit] ; [nat ; string ; unit] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_nat[] ;
     inferred z "=" C_unit[] ; ]
    (* Expected cleaned typeclass: *)
    [z]     "∈" [ [      int       ] ; [      string       ] ; ]
  ;

  test' "deduce_and_clean4" deduce_and_clean
    (* Input restricted typeclass: *)
    [x;z]   "∈" [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ; ]
    (* Expected inferred constraints: *)
    [inferred x "=" C_map[m;n] ; ] (* TODO: how to test this and not care about the actual m and n ? *)
    (* Expected cleaned typeclass: *)
    [m;n;z] "∈" [ [    nat;unit  ; int] ; [    unit;nat  ; string] ; ]
  ;
]

let main = test_suite "Typer: fundep heuriscic"
  @@ List.flatten
    [
      tests1 Typer_new.Heuristic_tc_fundep.restrict ;
      tests2 Typer_new.Heuristic_tc_fundep.deduce_and_clean ;
    ]
