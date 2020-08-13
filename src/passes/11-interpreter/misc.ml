open Ligo_interpreter.Types

let fail : string -> _ = fun s ->
  (*TODO This raise is here until we properly implement effects*)
  raise (Temporary_hack s)

let contract_not_found : string -> _ = fun addr -> 
  (* V_Failure ( *)
  fail @@
    Format.asprintf
    "Could not find address %s in the context" addr