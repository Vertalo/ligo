Printexc.record_backtrace true ;
Printf.printf "Start of mydebugplease executable\n%!";
let test_result = Typer_tests.TestExpressions.lambda () in
let _ = Printf.printf "TEST DONE\n%!" in
let test (x : unit Trace.result) : unit = match x with
| Ok (() , _annotation_thunk) -> ()
| Error err -> Printf.printf "\nMYDEBUGPLEASE ERROR:\n%s\n\n" @@ Yojson.Basic.to_string @@ err (); failwith "ERROR" in
let () = test test_result in
Printf.printf "End of mydebugplease executable\n%!"
