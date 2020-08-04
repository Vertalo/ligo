(* selector / propagation rule for restricting a type class
     (α₁, α₂, …) ∈ { (τ₁₁, τ₁₂, …) , … }
   to the possible cases, given a second hypothesis of the form
     αᵢ = κ(β₁, β₂, …)
   It restricts the number of possible cases and replaces αᵢ in
   tuple of constrained variables so that the βⱼ are constrained
   instead.

   This rule can deduce a new assignment for other variables
   constrained by the typeclass if every possible type for that
   variable uses the same type constructor. *)

(* TODO: have a heuristic that restricts typeclass constraints with
   repeated or aliased type variables in the arguments, i.e. of the
   form […;x;…;y;…] ∈ […] where x and y are identical or aliased. *)

module Core = Typesystem.Core
open Ast_typed.Types
open Typesystem.Solver_types
open Trace
open Typer_common.Errors
module Map = RedBlackTrees.PolyMap
module Set = RedBlackTrees.PolySet

(* This rule maintains in its private storage a representation of
   the latest version of each typeclass *)

let set_of_vars l = (Set.add_list l (Set.create ~cmp:Var.compare)).set
let make_refined_typeclass tcs : refined_typeclass = { tcs ; vars = set_of_vars tcs.args }
(* TODO: concise unique representant for typeclass constraints, for
   now use the entire typeclass and ignore the "reason" fields in the
   compare. *)
(* TODO: move to AST. *)
let typeclass_identifier_to_tc typeclass_identifier = typeclass_identifier
let compare_typeclass_identifier = Solver_should_be_generated.compare_c_typeclass_simpl

type typeclass_identifier = Ast_typed.c_typeclass_simpl
type 'v typeclass_identifierMap = (typeclass_identifier, 'v) RedBlackTrees.PolyMap.t
type refined_typeclass_typeclass_identifierMap = refined_typeclass typeclass_identifierMap
type typeclass_identifier_set = typeclass_identifier Set.t
type typeclass_identifier_set_map = typeclass_identifier_set typeVariableMap
type private_storage = {
  refined_typeclasses: refined_typeclass_typeclass_identifierMap ;
  typeclasses_constrained_by : typeclass_identifier_set_map ;
}

let splice f idx l =
  let rec splice acc f idx l =
    match l with
      [] -> failwith "invalid index into list"
    | hd :: tl ->
      if idx = 0
      then (List.append (List.rev acc) @@ List.append (f hd) @@ tl)
      else (splice (hd :: acc) f (idx - 1) tl)
  in splice [] f idx l

let splice_or_none f idx l =
  let rec splice_or_none acc f idx l =
    match l with
      [] -> failwith "invalid index into list"
    | hd :: tl ->
      if idx = 0
      then (match f hd with
          | None -> None
          | Some new_hds -> Some (List.append (List.rev acc) @@ List.append new_hds @@ tl))
      else (splice_or_none (hd :: acc) f (idx - 1) tl)
  in splice_or_none [] f idx l

let restrict_one (c : c_constructor_simpl) (allowed : type_value) =
  match c, allowed.t with
  | { reason_constr_simpl=_; tv=_; c_tag; tv_list }, P_constant { p_ctor_tag; p_ctor_args } ->
    if Ast_typed.Compare_generic.constant_tag c_tag p_ctor_tag = 0
    then if List.compare_lengths tv_list p_ctor_args = 0
      then Some p_ctor_args
      else None (* case removed because type constructors are different *)
    else None   (* case removed because argument lists are of different lengths *)
  | _, P_row _ -> failwith "TODO: support P_row similarly to P_constant"
  | _, (P_forall _ | P_variable _ | P_apply _) -> None (* TODO: does this mean that we can't satisfy these constraints? *)

(* Restricts a typeclass to the possible cases given v = k(a, …) in c *)
let restrict (({ reason_constr_simpl = _; tv = _; c_tag = _; tv_list } as c) : c_constructor_simpl) (tcs : c_typeclass_simpl) =
  (* TODO: this is bogus if there is shadowing *)
  let index = List.find_index (Var.equal c.tv) tcs.args in
  (* Eliminate the impossible cases and splice in the type arguments
     for the possible cases: *)
  let aux allowed_tuple =
    splice_or_none (fun allowed -> restrict_one c allowed) index allowed_tuple in
  let tc = List.filter_map aux tcs.tc in
  (* Replace the corresponding typeclass argument with the type
     variables passed to the type constructor *)
  let args = splice (fun _arg -> tv_list) index tcs.args in
  { reason_typeclass_simpl = tcs.reason_typeclass_simpl ; tc ; args }

(* Check that the typeclass is a rectangular matrix, with one column
   per argument. *)
let check_typeclass_rectangular ({ reason_typeclass_simpl=_; tc; args } as tcs : c_typeclass_simpl) =
  let nargs = List.length args in
  (* TODO: use the error monad for this internal error *)
  if (List.for_all (fun allowed -> List.length allowed = nargs) tc)
  then ok tcs
  else fail typeclass_not_a_rectangular_matrix

(* Check that the transposed typeclass is a rectangular matrix, with
   one row per argument. *)
let check_typeclass_transposed_rectangular (tc : (type_variable * type_value list) list) =
  match tc with
    [] -> ok tc
  | (_, hd) :: tl ->
    let hdlen = List.length hd in
    if List.for_all (fun (_, l) -> List.length l = hdlen) tl
    then ok tc
    else fail typeclass_not_a_rectangular_matrix

(* transpose ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                        [map(unit,nat) ; string ; ] ;
                        [map(int,int)  ; unit   ; ] ; ])
   will return [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                 z ? [ int           ; string        ; unit         ; ] ; ] *)
let transpose : c_typeclass_simpl -> ((type_variable * type_value list) list, _) result =
  fun { reason_typeclass_simpl = _; tc; args } ->
  (* TODO: List.map2 can raise an exception, catch it here and use the error monad to throw an internal error (all the lists of the typeclass should have the same length as the list of arguments. *)
  check_typeclass_transposed_rectangular @@
  List.fold_left
    (fun accs allowed_tuple ->
       List.map2 (fun (var, acc) allowed_type -> (var, allowed_type :: acc)) accs allowed_tuple)
    (List.map (fun var -> var, []) args)
    tc

(* transpose_back [ x ? [ map(nat,unit) ; map(unit,nat) ; map(int,int) ; ] ;
                    z ? [ int           ; string        ; unit         ; ] ; ]
   will return ([x;z] ∈ [ [map(nat,unit) ; int    ; ] ;
                          [map(unit,nat) ; string ; ] ;
                          [map(int,int)  ; unit   ; ] ; ]) *)
let transpose_back : _ -> (type_variable * type_value list) list -> (c_typeclass_simpl, _) result =
  fun reason_typeclass_simpl tcs ->
  (* TODO: List.map2 can raise an exception, catch it here and use the error monad to throw an internal error (all the lists of the typeclass should have the same length as the list of arguments. *)
  let tc =
    match tcs with
    | [] -> []
    | (_, hd_allowed_types) :: _ ->
      List.fold_left
        (fun allowed_tuples allowed_types ->
           List.map2 (fun allowed_tuple allowed_type -> allowed_type :: allowed_tuple) allowed_tuples allowed_types)
        (List.map (fun _ -> []) hd_allowed_types)
        (List.map snd tcs)
  in
  let args = List.map fst tcs in
  check_typeclass_rectangular @@
  { reason_typeclass_simpl; tc; args }

type 'a all_equal = Empty | All_equal_to of 'a | Different
let all_equal cmp = function
  | [] -> Empty
  | hd :: tl -> if List.for_all (fun x -> cmp x hd = 0) tl then All_equal_to hd else Different

let deduce_and_clean : c_typeclass_simpl -> (c_constructor_simpl list * c_typeclass_simpl, _) result = fun tcs ->
  (* ex. [x;z] ∈ [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ] *)
  let%bind tcs' = transpose tcs in
  (* ex. [ x ? [ map(nat,unit) ; map(unit,nat) ; ] ;
           z ? [ int           ; string        ; ] ; ] *)
  let aux ((x : type_variable) , (tvl : type_value list)) =
    let tags, argss = List.split @@ List.map
        (fun (tv : type_value) ->
           match tv.t with
           | P_constant { p_ctor_tag; p_ctor_args } -> p_ctor_tag, p_ctor_args
           | P_row { p_row_tag; p_row_args } -> ignore (p_row_tag, p_row_args); failwith "TODO: return p_row_tag, p_row_args similarly to P_constant"
           | P_forall _ ->
             (* In this case we would need to do specialization.
                For now we just leave as-is and don't deduce anything *)
             failwith "TODO"
           | P_variable _ ->
             (* In this case we  *)
             failwith "TODO"
           | P_apply _ ->
             (* In this case we would need to do β-reduction, if
                possible, or invoke another heuristic.
                For now we just leave as-is and don't deduce anything *)
             failwith "TODO"
        )
        tvl in
    match all_equal Ast_typed.Compare_generic.constant_tag tags with
    | Different -> ([x], [tvl], [])            (* Leave as-is, don't deduce anything *)
    | Empty ->
      (* TODO: keep track of the constraints used to refine the
         typeclass so far. *)
      failwith "type error: the typeclass does not allow any type for \
                this variable at this point"
    | All_equal_to c_tag ->
      match argss with
      | [] -> failwith "the typeclass does not allow any set of at this point"
      | (hd_args :: _) as argss ->
        let fresh_vars = List.map (fun _arg -> Var.fresh ~name:"todo" ()) hd_args in
        let deduced : c_constructor_simpl = {
          reason_constr_simpl = "inferred because it is the only remaining possibility at this point according to the typeclass [TODO]" ;
          tv = x;
          c_tag ;
          tv_list = fresh_vars
        } in
        (fresh_vars, argss, [deduced]) (* toss the identical tags, splice their arguments instead, and deduce the x = tag(?, …) constraint *)
  in
  let l = List.map aux tcs' in
  let (vars, tcs', deduced) = List.split3 @@ l in
  let vars = List.flatten @@ vars in
  let tcs' = List.flatten @@ tcs' in
  let deduced = List.flatten @@ deduced in
  let%bind tcs' = transpose_back tcs.reason_typeclass_simpl (List.combine vars tcs' (* TODO: is this correct? *)) in
  ok (deduced, tcs')

module Test = struct
  let mk p_ctor_tag p_ctor_args = { tsrc = "unit test"; t = P_constant { p_ctor_tag ; p_ctor_args ; } ; }
  (* A bunch of arbitrary types *)
  let (int, unit, nat, string) = (mk C_int [], mk C_unit [], mk C_nat [], mk C_string [])
  (* An arbitrary two-argument type constructor. *)
  let map (k,v) = mk C_map [k; v]
  (* A bunch of type variables: *)
  let (m,n,x,y,z) = let v name = Var.fresh ~name () in v "m", v "n", v "x", v "y", v "z"

  let test
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
    let info = { reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list } in
    let tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
    let expected =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
    assert (Ast_typed.Compare_generic.c_typeclass_simpl (restrict info tc) expected = 0)

  let tests1 restrict =
    test restrict
      (* New info: a variable assignment constraint: *)
      x "=" C_nat[]
      (* Initial typeclass constraint: *)
      [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
      (* Intermediate step (not tested): *)
      (**)        [ None               ;  Some []          ;  Some []             ; ]
      (* Expected restricted typeclass: *)
      [y;z]   "∈" [                      [      int ; int] ; [      int ; string] ; ]
    ;

    test restrict
      (* New info: a variable assignment constraint: *)
      x "=" C_map[m;n]
      (* Initial typeclass constraint: *)
      [x;y]   "∈" [[int  ; unit] ; [map(nat,nat)   ; int] ; [map(nat,string)   ; int] ; ]
      (* Intermediate step (not tested): *)
      (**)        [ None         ;  Some [nat;nat]        ;  Some [nat;string]        ; ]
      (* Expected restricted typeclass constraint: *)
      [m;n;y] "∈" [                [nat ; nat      ; int] ; [nat ; string      ; int] ; ]
    ;

    test restrict
      (* New info: a variable assignment constraint: *)
      y "=" C_nat[]
      (* Initial typeclass constraint: *)
      [x;y;z] "∈" [[int ; unit ; unit] ; [nat ; int ; int] ; [nat ; int ; string] ; ]
      (* Intermediate step (not tested): *)
      (**)        [       None         ;        Some []    ;        Some []       ; ]
      (* Expected restricted typeclass: *)
      [x;z]   "∈" [                      [nat ;       int] ; [nat ;       string] ; ]
    ;    

    ()

  let test'
      (deduce_and_clean : c_typeclass_simpl -> (c_constructor_simpl list * c_typeclass_simpl, _) result)
      args (_in : string) tc
      (expected_inferred  : (type_variable * constant_tag * type_variable list) list)
      expected_args (_in : string) expected_tc =
    let input_tc =  { reason_typeclass_simpl = "unit test" ; args ; tc } in
    let expected_tc =  { reason_typeclass_simpl = "unit test" ; args = expected_args ; tc = expected_tc } in
    let expected_inferred = List.map
        (fun (tv , c_tag , tv_list) -> {reason_constr_simpl = "unit test" ; tv ; c_tag ; tv_list})
        expected_inferred in
    match deduce_and_clean input_tc with
    | Error _ ->
      assert false
    | Stdlib.Ok ((actual_inferred , actual_tc), _) ->
      assert (Ast_typed.Compare_generic.c_constructor_simpl_list actual_inferred expected_inferred = 0);
      assert (Ast_typed.Compare_generic.c_typeclass_simpl actual_tc expected_tc = 0);
      ()
  let inferred v (_eq : string) c args = v, c, args
  let tests2 deduce_and_clean =
    test' deduce_and_clean
      (* Input restricted typeclass: *)
      [x;z]   "∈" [ [nat ; int] ; [nat ; string] ; ]
      (* Expected inferred constraints: *)
      [inferred x "=" C_nat[] ; ]
      (* Expected cleaned typeclass: *)
      [z]     "∈" [ [      int] ; [      string] ; ]
    ;

    test' deduce_and_clean
      (* Input restricted typeclass: *)
      [x;y;z] "∈" [ [nat ; int ; unit] ; [nat ; string ; unit] ; ]
      (* Expected inferred constraints: *)
      [inferred x "=" C_nat[] ;
       inferred z "=" C_unit[] ; ]
      (* Expected cleaned typeclass: *)
      [z]     "∈" [ [      int       ] ; [      string       ] ; ]
    ;

    test' deduce_and_clean
      (* Input restricted typeclass: *)
      [x;z]   "∈" [ [map(nat,unit) ; int] ; [map(unit,nat) ; string] ; ]
      (* Expected inferred constraints: *)
      [inferred x "=" C_map[m;n] ; ] (* TODO: how to test this and not care about the actual m and n ? *)
      (* Expected cleaned typeclass: *)
      [m;n;z] "∈" [ [    nat;unit  ; int] ; [    unit;nat  ; string] ; ]
    ;

    ()

  let () =
    tests1 restrict ;
    tests2 deduce_and_clean ;
    ()
end

let get_or_add_refined_typeclass tc { refined_typeclasses; typeclasses_constrained_by }
  : (refined_typeclass * private_storage) =
  match PolyMap.find_opt tc refined_typeclasses with
    None ->
    let rtc = make_refined_typeclass tc in
    let refined_typeclasses = PolyMap.add tc rtc refined_typeclasses in
    let aux' set = Some (Set.add tc (match set with Some set -> set | None -> Set.create ~cmp:compare_typeclass_identifier)) in
    let aux = (fun typeclasses_constrained_by tv ->
           Map.update tv (aux') typeclasses_constrained_by) in
    let typeclasses_constrained_by =
      List.fold_left
        aux
        typeclasses_constrained_by
        tc.args in
    rtc, { refined_typeclasses; typeclasses_constrained_by }
  | Some rtc ->
    rtc, { refined_typeclasses; typeclasses_constrained_by }

let selector_by_ctor (private_storage : private_storage) (dbs : structured_dbs) (c : c_constructor_simpl) : _ = 
    (* find a typeclass in db which constrains c.tv *)
    let other_cs = (Constraint_databases.get_constraints_related_to c.tv dbs).tc in
    let other_cs = List.filter (fun (x : c_typeclass_simpl) -> List.exists (Var.equal c.tv) x.args) other_cs in
    let cs_pairs_db = List.fold_map (fun ps tc -> let tc, ps = get_or_add_refined_typeclass tc ps in ps, { tc ; c }) private_storage other_cs in
    
    (* find a typeclass in refined_typeclasses which constrains c.tv *)
    let other_cs = Map.find c.tv private_storage.typeclasses_constrained_by in
    let cs_pairs_refined = List.map (fun tc -> { tc = make_refined_typeclass tc ; c }) @@ Set.elements other_cs in

    private_storage, WasSelected (cs_pairs_db @ cs_pairs_refined)

let selector_by_tc (private_storage : private_storage) (dbs : structured_dbs) (tc : c_typeclass_simpl) : _ = 
    (* This case finds the constructor constraints which apply to the variables
       constrained by the typeclass. *)
    let tc, private_storage = get_or_add_refined_typeclass tc private_storage in
    (* TODO: this won't detect already-existing constructor
       constraints that would apply to future versions of the refined
       typeclass. *)
    let aux tv =
      (* Find the constructor constraints which apply to tv. *)
      (* Since we are only refining the typeclass one type expression
         node at a time, we only need the top-level assignment for
         that variable, e.g. α = κ(βᵢ, …). We can therefore look
         directly in the assignments.

         TODO: check that we don't miss any constructor constraints
         that way. *)
      let assignment = PolyMap.find_opt tv dbs.assignments in
      match assignment with
        Some c -> [({ tc ; c } : output_tc_fundep)]
      | None   -> [] in
    private_storage, WasSelected (List.flatten @@ List.map aux tc.tcs.args)

let selector : (type_constraint_simpl , output_tc_fundep , private_storage) selector =
  fun type_constraint_simpl private_storage dbs ->
  match type_constraint_simpl with
    SC_Constructor c  -> selector_by_ctor private_storage dbs c
  | SC_Row r          -> ignore r; failwith "TODO: call selector_by_ctor private_storage dbs r"
  | SC_Alias       _  -> private_storage, WasNotSelected (* TODO: ? *)
  | SC_Poly        _  -> private_storage, WasNotSelected (* TODO: ? *)
  | SC_Typeclass   tc -> selector_by_tc private_storage dbs tc

let propagator : (output_tc_fundep, private_storage , typer_error) propagator =
  fun private_storage _dbs selected ->
  (* The selector is expected to provide constraints with the shape (α
     = κ(β, …)) and to update the private storage to keep track of the
     refined typeclass *)
  let restricted = restrict selected.c selected.tc.tcs in
  let%bind (constraints, cleaned) = deduce_and_clean restricted in
  let cleaned : refined_typeclass = make_refined_typeclass cleaned in
  let refined_typeclasses = PolyMap.add selected.tc.tcs cleaned private_storage.refined_typeclasses in
  let typeclasses_constrained_by =
    ( failwith "TODO" ) (* update private_storage.typeclasses_constrained_by *) in
  let private_storage : private_storage = { refined_typeclasses ; typeclasses_constrained_by } in
  let aux (x : c_constructor_simpl) : type_constraint = {
    reason = "inferred: only possible type for that variable in the typeclass";
    c = C_equation {
        aval = { tsrc = "?" ;
                 t    = P_variable x.tv };
        bval = { tsrc = "? generated" ;
                 t    = P_constant { p_ctor_tag  = x.c_tag ;
                                     p_ctor_args =
                                       List.map
                                         (fun v -> { tsrc = "? probably generated" ;
                                                     t    = P_variable v})
                                         x.tv_list ; } } } } in
  let constraints : type_constraint list = List.map aux constraints in
  ok (private_storage, constraints)

let heuristic =
  Propagator_heuristic
    {
      selector ;
      propagator ;
      printer = Ast_typed.PP_generic.output_tc_fundep ;
      comparator = Solver_should_be_generated.compare_output_tc_fundep ;
      initial_private_storage = {
        refined_typeclasses = Map.create ~cmp:compare_typeclass_identifier ;
        typeclasses_constrained_by = Map.create ~cmp:Var.compare ;
      } ;
    }

