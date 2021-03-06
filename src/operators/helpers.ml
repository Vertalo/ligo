module Typer = struct

  open Trace
  open Ast_typed

  module Errors = struct
    let wrong_param_number = fun name expected got ->
      let title () = "wrong number of params" in
      let full () = Format.asprintf "constant name: %s\nexpected: %d\ngot: %d\n"
          name expected (List.length got) in
      error title full
  end


  type type_result = string * type_value
  type typer' = type_value list -> type_value option -> type_result result
  type typer = string * typer'

  let typer'_0 : name -> (type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [] -> (
      let%bind tv' = f tv_opt in
      ok (s , tv')
    )
    | _ -> fail @@ Errors.wrong_param_number s 0 lst
  let typer_0 name f : typer = (name , typer'_0 name f)

  let typer'_1 : name -> (type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 1 lst
  let typer_1 name f : typer = (name , typer'_1 name f)

  let typer'_1_opt : name -> (type_value -> type_value option -> type_value result) -> typer' = fun s f lst tv_opt ->
    match lst with
    | [ a ] -> (
        let%bind tv' = f a tv_opt in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 1 lst
  let typer_1_opt name f : typer = (name , typer'_1_opt name f)

  let typer'_2 : name -> (type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ] -> (
        let%bind tv' = f a b in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 2 lst
  let typer_2 name f : typer = (name , typer'_2 name f)

  let typer'_3 : name -> (type_value -> type_value -> type_value -> type_value result) -> typer' = fun s f lst _ ->
    match lst with
    | [ a ; b ; c ] -> (
        let%bind tv' = f a b c in
        ok (s , tv')
      )
    | _ -> fail @@ Errors.wrong_param_number s 3 lst
  let typer_3 name f : typer = (name , typer'_3 name f)

  let constant name cst = typer_0 name (fun _ -> ok cst)

  open Combinators

  let eq_1 a cst = type_value_eq (a , cst)
  let eq_2 (a , b) cst = type_value_eq (a , cst) && type_value_eq (b , cst)

  let comparator : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "Types a and b aren't comparable") @@
      Assert.assert_true @@
      List.exists (eq_2 (a , b)) [
        t_int () ;
        t_nat () ;
        t_tez () ;
        t_string () ;
        t_bytes () ;
        t_address () ;
      ] in
    ok @@ t_bool ()

  let boolean_operator_2 : string -> typer = fun s -> typer_2 s @@ fun a b ->
    let%bind () =
      trace_strong (simple_error "A isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , a) in
    let%bind () =
      trace_strong (simple_error "B isn't of type bool") @@
      Assert.assert_true @@
      type_value_eq (t_bool () , b) in
    ok @@ t_bool ()

end

module Compiler = struct

  open Tezos_utils.Michelson

  type predicate =
    | Constant of michelson
    | Unary of michelson
    | Binary of michelson
    | Ternary of michelson
  let simple_constant c = Constant c
  let simple_unary c = Unary c
  let simple_binary c = Binary c
  let simple_ternary c = Ternary c
end
