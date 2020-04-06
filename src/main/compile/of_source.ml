open Trace
open Helpers

let compile (source_filename : string) syntax : Ast_imperative.program result =
  let%bind syntax = syntax_to_variant syntax (Some source_filename) in
  let%bind abstract = parsify syntax source_filename in
  ok abstract

let compile_string (source : string) syntax : Ast_imperative.program result =
  let%bind abstract = parsify_string syntax source in
  ok abstract

let compile_expression : v_syntax -> string -> Ast_imperative.expression result
    =
 fun syntax exp -> parsify_expression syntax exp

let compile_contract_input :
    string -> string -> v_syntax -> Ast_imperative.expression result =
 fun storage parameter syntax ->
  let%bind (storage, parameter) =
    bind_map_pair (compile_expression syntax) (storage, parameter)
  in
  ok @@ Ast_imperative.e_pair storage parameter

let pretty_print source_filename syntax =
  Helpers.pretty_print syntax source_filename
