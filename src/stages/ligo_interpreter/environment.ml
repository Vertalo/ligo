open Trace
open Types

let extend :
  env -> (expression_variable * value) -> env
    = fun env (var,exp) -> Env.add var exp env

let lookup :
  env -> expression_variable -> (value,_) result
    = fun env var -> match Env.find_opt var env with
      | Some res -> ok res
      | None -> failwith "TODO: not found in env"

let empty_env = Env.empty