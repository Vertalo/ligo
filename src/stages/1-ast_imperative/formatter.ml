open Display

let program_ppformat ~display_format f (p,_) =
  match display_format with
  | Human_readable | Dev -> PP.program f p

let program_jsonformat (p,_) : json =
  let p' = Format.asprintf "%a" PP.program p in
  `Assoc [("AST" , `String p')]

let program_format : 'a format = {
  pp = program_ppformat;
  to_json = program_jsonformat;
}