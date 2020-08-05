open Ast_typed.Types
module Set = RedBlackTrees.PolySet

type 'old_constraint_type selector_input = 'old_constraint_type (* some info about the constraint just added, so that we know what to look for *)
type 'selector_output selector_outputs =
    WasSelected of 'selector_output list
  | WasNotSelected
type new_constraints = type_constraint list
type new_assignments = c_constructor_simpl list
type ('old_constraint_type, 'selector_output) selector = 'old_constraint_type selector_input -> structured_dbs -> 'selector_output selector_outputs
type 'selector_output propagator = structured_dbs -> 'selector_output -> new_constraints * new_assignments
type ('old_constraint_type , 'selector_output ) propagator_heuristic = {
  (* sub-sub component: lazy selector (don't re-try all selectors every time)
   * For now: just re-try everytime *)
  selector          : ('old_constraint_type, 'selector_output) selector ;
  (* constraint propagation: (buch of constraints) → (new constraints * assignments) *)
  propagator        : 'selector_output propagator ;
  printer           : Format.formatter -> 'selector_output -> unit ;
  comparator        : 'selector_output -> 'selector_output -> int ;
}

type ('old_constraint_type , 'selector_output ) propagator_state = {
  selector          : ('old_constraint_type, 'selector_output) selector ;
  propagator        : 'selector_output propagator ;
  printer           : Format.formatter -> 'selector_output -> unit ;
  already_selected  : 'selector_output Set.t;
}

type ex_propagator_heuristic =
  (* For now only support a single type of input, make this polymorphic as needed. *)
  | Propagator_heuristic : (type_constraint_simpl, 'selector_output) propagator_heuristic -> ex_propagator_heuristic

type ex_propagator_state =
  (* For now only support a single type of input, make this polymorphic as needed. *)
  | Propagator_state : (type_constraint_simpl, 'selector_output) propagator_state -> ex_propagator_state

type typer_state = {
  structured_dbs                   : structured_dbs   ;
  already_selected_and_propagators : ex_propagator_state list ;
}

open Format
open PP_helpers

let pp_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "Set [@,@[<hv 2> %a @]@,]" (list_sep printer (fun ppf () -> fprintf ppf " ;@ ")) lst

let pp_ex_propagator_state = fun ppf (Propagator_state { selector ; propagator ; printer ; already_selected }) ->
  ignore ( selector, propagator );
  Format.fprintf ppf "{ selector = (* OCaml function *); propagator = (* OCaml function *); already_selected = %a }"
  (pp_already_selected printer) already_selected

let pp_typer_state = fun ppf ({ structured_dbs; already_selected_and_propagators } : typer_state) ->
  Format.fprintf ppf "{ structured_dbs = %a ; already_selected_and_propagators = [ %a ] }"
    Ast_typed.PP_generic.structured_dbs structured_dbs
    (list_sep pp_ex_propagator_state (fun ppf () -> fprintf ppf " ;@ ")) already_selected_and_propagators


let json_already_selected = fun printer ppf set ->
  let lst = (RedBlackTrees.PolySet.elements set) in
    Format.fprintf ppf "[ \"Set\" %a ]" (list_sep printer (fun ppf () -> fprintf ppf " , ")) lst

let json_ex_propagator_state = fun ppf (Propagator_state { selector; propagator; printer ; already_selected }) ->
  ignore (selector,propagator);
  Format.fprintf ppf "{ \"selector\": \"OCaml function\"; \"propagator\": \"OCaml function\"; \"already_selected\": %a }"
  (json_already_selected printer) already_selected

let json_typer_state = fun ppf ({ structured_dbs; already_selected_and_propagators } : typer_state) ->
  Format.fprintf ppf "{ \"structured_dbs\": %a ; \"already_selected_and_propagators\": [ %a ] }"
    Ast_typed.PP_json.structured_dbs structured_dbs
    (list_sep json_ex_propagator_state (fun ppf () -> fprintf ppf " , ")) already_selected_and_propagators

(* state+list monad *)
type ('state, 'elt) state_list_monad = { state: 'state ; list : 'elt list }
let lift_state_list_monad ~state ~list = { state ; list }
let lift f =
  fun { state ; list } ->
    let (new_state , new_lists) = List.fold_map_acc f state list in
    { state = new_state ; list = List.flatten new_lists }
