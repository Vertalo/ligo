(* Lexer specification for LIGO, to be processed by [ocamllex] *)

{
(* START HEADER *)

(* Shorthands *)

type lexeme = string

let sprintf = Printf.sprintf

module Region = Simple_utils.Region
module Pos    = Simple_utils.Pos
module SMap   = Utils.String.Map
module SSet   = Utils.String.Set

(* Hack to roll back one lexeme in the current semantic action *)
(*
let rollback buffer =
  let open Lexing in
  let len = String.length (lexeme buffer) in
  let pos_cnum = buffer.lex_curr_p.pos_cnum - len in
  buffer.lex_curr_pos <- buffer.lex_curr_pos - len;
  buffer.lex_curr_p <- {buffer.lex_curr_p with pos_cnum}
*)

(* TOKENS *)

type t =
  (* Literals *)

  String of lexeme Region.reg
| Bytes  of (lexeme * Hex.t) Region.reg
| Int    of (lexeme * Z.t) Region.reg
| Nat    of (lexeme * Z.t) Region.reg
| Mtz    of (lexeme * Z.t) Region.reg
| Ident  of lexeme Region.reg
| Constr of lexeme Region.reg

  (* Symbols *)

| SEMI     of Region.t
| COMMA    of Region.t
| LPAR     of Region.t
| RPAR     of Region.t
| LBRACE   of Region.t
| RBRACE   of Region.t
| LBRACKET of Region.t
| RBRACKET of Region.t
| CONS     of Region.t
| VBAR     of Region.t
| ARROW    of Region.t
| ASS      of Region.t
| EQUAL    of Region.t
| COLON    of Region.t
| LT       of Region.t
| LEQ      of Region.t
| GT       of Region.t
| GEQ      of Region.t
| NEQ      of Region.t
| PLUS     of Region.t
| MINUS    of Region.t
| SLASH    of Region.t
| TIMES    of Region.t
| DOT      of Region.t
| WILD     of Region.t
| CAT      of Region.t

  (* Keywords *)

| And        of Region.t  (* "and"        *)
| Begin      of Region.t  (* "begin"      *)
| Block      of Region.t  (* "block"      *)
| Case       of Region.t  (* "case"       *)
| Const      of Region.t  (* "const"      *)
| Contains   of Region.t  (* "contains"   *)
| Down       of Region.t  (* "down"       *)
| Else       of Region.t  (* "else"       *)
| End        of Region.t  (* "end"        *)
| Fail       of Region.t  (* "fail"       *)
| For        of Region.t  (* "for"        *)
| From       of Region.t  (* "from"       *)
| Function   of Region.t  (* "function"   *)
| If         of Region.t  (* "if"         *)
| In         of Region.t  (* "in"         *)
| Is         of Region.t  (* "is"         *)
| List       of Region.t  (* "list"       *)
| Map        of Region.t  (* "map"        *)
| Mod        of Region.t  (* "mod"        *)
| Nil        of Region.t  (* "nil"        *)
| Not        of Region.t  (* "not"        *)
| Of         of Region.t  (* "of"         *)
| Or         of Region.t  (* "or"         *)
| Patch      of Region.t  (* "patch"      *)
| Procedure  of Region.t  (* "procedure"  *)
| Record     of Region.t  (* "record"     *)
| Remove     of Region.t  (* "remove"     *)
| Set        of Region.t  (* "set"        *)
| Skip       of Region.t  (* "skip"       *)
| Step       of Region.t  (* "step"       *)
| Then       of Region.t  (* "then"       *)
| To         of Region.t  (* "to"         *)
| Type       of Region.t  (* "type"       *)
| Var        of Region.t  (* "var"        *)
| While      of Region.t  (* "while"      *)
| With       of Region.t  (* "with"       *)

  (* Types *)
(*
| T_address   of Region.t  (* "address"   *)
| T_big_map   of Region.t  (* "big_map"   *)
| T_bool      of Region.t  (* "bool"      *)
| T_bytes     of Region.t  (* "bytes"     *)
| T_contract  of Region.t  (* "contract"  *)
| T_int       of Region.t  (* "int"       *)
| T_key       of Region.t  (* "key"       *)
| T_key_hash  of Region.t  (* "key_hash"  *)
| T_list      of Region.t  (* "list"      *)
| T_map       of Region.t  (* "map"       *)
| T_mutez     of Region.t  (* "mutez"     *)
| T_nat       of Region.t  (* "nat"       *)
| T_operation of Region.t  (* "operation" *)
| T_option    of Region.t  (* "option"    *)
| T_set       of Region.t  (* "set"       *)
| T_signature of Region.t  (* "signature" *)
| T_string    of Region.t  (* "string"    *)
| T_timestamp of Region.t  (* "timestamp" *)
| T_unit      of Region.t  (* "unit"      *)
*)
  (* Data constructors *)

| C_False of Region.t  (* "False" *)
| C_None  of Region.t  (* "None"  *)
| C_Some  of Region.t  (* "Some"  *)
| C_True  of Region.t  (* "True"  *)
| C_Unit  of Region.t  (* "Unit"  *)

  (* Virtual tokens *)

| EOF of Region.t


type token = t

let proj_token = function
  (* Literals *)

  String Region.{region; value} ->
    region, sprintf "String %s" value

| Bytes Region.{region; value = s,b} ->
    region,
    sprintf "Bytes (\"%s\", \"0x%s\")"
      s (Hex.to_string b)

| Int Region.{region; value = s,n} ->
    region, sprintf "Int (\"%s\", %s)" s (Z.to_string n)

| Nat Region.{region; value = s,n} ->
    region, sprintf "Nat (\"%s\", %s)" s (Z.to_string n)

| Mtz Region.{region; value = s,n} ->
    region, sprintf "Mtz (\"%s\", %s)" s (Z.to_string n)

| Ident Region.{region; value} ->
    region, sprintf "Ident \"%s\"" value

| Constr Region.{region; value} ->
    region, sprintf "Constr \"%s\"" value

  (* Symbols *)

| SEMI     region -> region, "SEMI"
| COMMA    region -> region, "COMMA"
| LPAR     region -> region, "LPAR"
| RPAR     region -> region, "RPAR"
| LBRACE   region -> region, "LBRACE"
| RBRACE   region -> region, "RBRACE"
| LBRACKET region -> region, "LBRACKET"
| RBRACKET region -> region, "RBRACKET"
| CONS     region -> region, "CONS"
| VBAR     region -> region, "VBAR"
| ARROW    region -> region, "ARROW"
| ASS      region -> region, "ASS"
| EQUAL    region -> region, "EQUAL"
| COLON    region -> region, "COLON"
| LT       region -> region, "LT"
| LEQ      region -> region, "LEQ"
| GT       region -> region, "GT"
| GEQ      region -> region, "GEQ"
| NEQ      region -> region, "NEQ"
| PLUS     region -> region, "PLUS"
| MINUS    region -> region, "MINUS"
| SLASH    region -> region, "SLASH"
| TIMES    region -> region, "TIMES"
| DOT      region -> region, "DOT"
| WILD     region -> region, "WILD"
| CAT      region -> region, "CAT"

  (* Keywords *)

| And        region -> region, "And"
| Begin      region -> region, "Begin"
| Block      region -> region, "Block"
| Case       region -> region, "Case"
| Const      region -> region, "Const"
| Contains   region -> region, "Contains"
| Down       region -> region, "Down"
| Else       region -> region, "Else"
| End        region -> region, "End"
| Fail       region -> region, "Fail"
| For        region -> region, "For"
| From       region -> region, "From"
| Function   region -> region, "Function"
| If         region -> region, "If"
| In         region -> region, "In"
| Is         region -> region, "Is"
| List       region -> region, "List"
| Map        region -> region, "Map"
| Mod        region -> region, "Mod"
| Nil        region -> region, "Nil"
| Not        region -> region, "Not"
| Of         region -> region, "Of"
| Or         region -> region, "Or"
| Patch      region -> region, "Patch"
| Procedure  region -> region, "Procedure"
| Record     region -> region, "Record"
| Remove     region -> region, "Remove"
| Set        region -> region, "Set"
| Skip       region -> region, "Skip"
| Step       region -> region, "Step"
| Then       region -> region, "Then"
| To         region -> region, "To"
| Type       region -> region, "Type"
| Var        region -> region, "Var"
| While      region -> region, "While"
| With       region -> region, "With"

  (* Data *)

| C_False region -> region, "C_False"
| C_None  region -> region, "C_None"
| C_Some  region -> region, "C_Some"
| C_True  region -> region, "C_True"
| C_Unit  region -> region, "C_Unit"

  (* Virtual tokens *)

| EOF region -> region, "EOF"


let to_lexeme = function
  (* Literals *)

  String s  -> s.Region.value
| Bytes b   -> fst b.Region.value
| Int i
| Nat i
| Mtz i     -> fst i.Region.value
| Ident id
| Constr id -> id.Region.value

  (* Symbols *)

| SEMI     _ -> ";"
| COMMA    _ -> ","
| LPAR     _ -> "("
| RPAR     _ -> ")"
| LBRACE   _ -> "{"
| RBRACE   _ -> "}"
| LBRACKET _ -> "["
| RBRACKET _ -> "]"
| CONS     _ -> "#"
| VBAR     _ -> "|"
| ARROW    _ -> "->"
| ASS      _ -> ":="
| EQUAL    _ -> "="
| COLON    _ -> ":"
| LT       _ -> "<"
| LEQ      _ -> "<="
| GT       _ -> ">"
| GEQ      _ -> ">="
| NEQ      _ -> "=/="
| PLUS     _ -> "+"
| MINUS    _ -> "-"
| SLASH    _ -> "/"
| TIMES    _ -> "*"
| DOT      _ -> "."
| WILD     _ -> "_"
| CAT      _ -> "^"

  (* Keywords *)

| And        _ -> "and"
| Begin      _ -> "begin"
| Block      _ -> "block"
| Case       _ -> "case"
| Const      _ -> "const"
| Contains   _ -> "contains"
| Down       _ -> "down"
| Else       _ -> "else"
| End        _ -> "end"
| Fail       _ -> "fail"
| For        _ -> "for"
| From       _ -> "from"
| Function   _ -> "function"
| If         _ -> "if"
| In         _ -> "in"
| Is         _ -> "is"
| List       _ -> "list"
| Map        _ -> "map"
| Mod        _ -> "mod"
| Nil        _ -> "nil"
| Not        _ -> "not"
| Of         _ -> "of"
| Or         _ -> "or"
| Patch      _ -> "patch"
| Procedure  _ -> "procedure"
| Record     _ -> "record"
| Remove     _ -> "remove"
| Set        _ -> "set"
| Skip       _ -> "skip"
| Step       _ -> "step"
| Then       _ -> "then"
| To         _ -> "to"
| Type       _ -> "type"
| Var        _ -> "var"
| While      _ -> "while"
| With       _ -> "with"

  (* Data constructors *)

| C_False _ -> "False"
| C_None  _ -> "None"
| C_Some  _ -> "Some"
| C_True  _ -> "True"
| C_Unit  _ -> "Unit"

  (* Virtual tokens *)

| EOF _ -> ""


let to_string token ?(offsets=true) mode =
  let region, val_str = proj_token token in
  let reg_str = region#compact ~offsets mode
  in sprintf "%s: %s" reg_str val_str

let to_region token = proj_token token |> fst

(* LEXIS *)

let keywords = [
  (fun reg -> And        reg);
  (fun reg -> Begin      reg);
  (fun reg -> Block      reg);
  (fun reg -> Case       reg);
  (fun reg -> Const      reg);
  (fun reg -> Contains   reg);
  (fun reg -> Down       reg);
  (fun reg -> Else       reg);
  (fun reg -> End        reg);
  (fun reg -> For        reg);
  (fun reg -> From       reg);
  (fun reg -> Function   reg);
  (fun reg -> Fail       reg);
  (fun reg -> If         reg);
  (fun reg -> In         reg);
  (fun reg -> Is         reg);
  (fun reg -> List       reg);
  (fun reg -> Map        reg);
  (fun reg -> Mod        reg);
  (fun reg -> Nil        reg);
  (fun reg -> Not        reg);
  (fun reg -> Of         reg);
  (fun reg -> Or         reg);
  (fun reg -> Patch      reg);
  (fun reg -> Procedure  reg);
  (fun reg -> Record     reg);
  (fun reg -> Remove     reg);
  (fun reg -> Set        reg);
  (fun reg -> Skip       reg);
  (fun reg -> Step       reg);
  (fun reg -> Then       reg);
  (fun reg -> To         reg);
  (fun reg -> Type       reg);
  (fun reg -> Var        reg);
  (fun reg -> While      reg);
  (fun reg -> With       reg)
]

let reserved =
  let open SSet in
  empty |> add "args"

let constructors = [
  (fun reg -> C_False reg);
  (fun reg -> C_None  reg);
  (fun reg -> C_Some  reg);
  (fun reg -> C_True  reg);
  (fun reg -> C_Unit  reg)
]

let add map (key, value) = SMap.add key value map

let mk_map mk_key list =
  let apply map value = add map (mk_key value, value)
  in List.fold_left apply SMap.empty list

type lexis = {
  kwd  : (Region.t -> token) SMap.t;
  cstr : (Region.t -> token) SMap.t;
  res  : SSet.t
}

let lexicon : lexis =
  let build list = mk_map (fun f -> to_lexeme (f Region.ghost)) list
  in {kwd  = build keywords;
      cstr = build constructors;
      res  = reserved}

(* Identifiers *)

type ident_err = Reserved_name

(* END HEADER *)
}

(* START LEXER DEFINITION *)

(* Named regular expressions *)

let small   = ['a'-'z']
let capital = ['A'-'Z']
let letter  = small | capital
let digit   = ['0'-'9']
let ident   = small (letter | '_' | digit)*
let constr  = capital (letter | '_' | digit)*

(* Rules *)

rule scan_ident region lexicon = parse
  (ident as value) eof {
    if   SSet.mem value lexicon.res
    then Error Reserved_name
    else Ok (match SMap.find_opt value lexicon.kwd with
               Some mk_kwd -> mk_kwd region
             |        None -> Ident Region.{region; value}) }

and scan_constr region lexicon = parse
  (constr as value) eof {
    match SMap.find_opt value lexicon.cstr with
      Some mk_cstr -> mk_cstr region
    |         None -> Constr Region.{region; value} }

(* END LEXER DEFINITION *)

{
(* START TRAILER *)

(* Smart constructors (injections) *)

let mk_string lexeme region = String Region.{region; value=lexeme}

let mk_bytes lexeme region =
  let norm = Str.(global_replace (regexp "_") "" lexeme) in
  let value = lexeme, Hex.of_string norm
  in Bytes Region.{region; value}

type int_err = Non_canonical_zero

let mk_int lexeme region =
  let z = Str.(global_replace (regexp "_") "" lexeme)
          |> Z.of_string in
  if   Z.equal z Z.zero && lexeme <> "0"
  then Error Non_canonical_zero
  else Ok (Int Region.{region; value = lexeme, z})

type invalid_natural = 
  | Invalid_natural
  | Non_canonical_zero_nat

let mk_nat lexeme region =
  match (String.index_opt lexeme 'n') with  
  | None -> Error Invalid_natural
  | Some _ -> (  
    let z =
      Str.(global_replace (regexp "_") "" lexeme) |>
      Str.(global_replace (regexp "n") "") |>
      Z.of_string in
    if Z.equal z Z.zero && lexeme <> "0n"
    then Error Non_canonical_zero_nat
    else Ok (Nat Region.{region; value = lexeme, z})
  )

let mk_mtz lexeme region =
  let z =
    Str.(global_replace (regexp "_") "" lexeme) |>
    Str.(global_replace (regexp "mtz") "") |>
    Z.of_string in
  if Z.equal z Z.zero && lexeme <> "0mtz"
  then Error Non_canonical_zero
  else Ok (Mtz Region.{region; value = lexeme, z})

let eof region = EOF region

let mk_sym lexeme region =
  match lexeme with
    ";"   -> SEMI     region
  | ","   -> COMMA    region
  | "("   -> LPAR     region
  | ")"   -> RPAR     region
  | "{"   -> LBRACE   region
  | "}"   -> RBRACE   region
  | "["   -> LBRACKET region
  | "]"   -> RBRACKET region
  | "#"   -> CONS     region
  | "|"   -> VBAR     region
  | "->"  -> ARROW    region
  | ":="  -> ASS      region
  | "="   -> EQUAL    region
  | ":"   -> COLON    region
  | "<"   -> LT       region
  | "<="  -> LEQ      region
  | ">"   -> GT       region
  | ">="  -> GEQ      region
  | "=/=" -> NEQ      region
  | "+"   -> PLUS     region
  | "-"   -> MINUS    region
  | "/"   -> SLASH    region
  | "*"   -> TIMES    region
  | "."   -> DOT      region
  | "_"   -> WILD     region
  | "^"   -> CAT      region
  |     _ -> assert false

(* Identifiers *)

let mk_ident' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_ident region lexicon

let mk_ident lexeme region = mk_ident' lexeme region lexicon

(* Constructors *)

let mk_constr' lexeme region lexicon =
  Lexing.from_string lexeme |> scan_constr region lexicon

let mk_constr lexeme region = mk_constr' lexeme region lexicon

(* Predicates *)

let is_string = function
  String _ -> true
|        _ -> false

let is_bytes = function
  Bytes _ -> true
|       _ -> false

let is_int = function
  Int _ -> true
|     _ -> false

let is_ident = function
  Ident _ -> true
|       _ -> false

let is_kwd = function
  And        _
| Begin      _
| Block      _
| Case       _
| Const      _
| Contains   _
| Down       _
| Else       _
| End        _
| Fail       _
| For        _
| From       _
| Function   _
| If         _
| In         _
| Is         _
| List       _
| Map        _
| Mod        _
| Nil        _
| Not        _
| Of         _
| Or         _
| Patch      _
| Procedure  _
| Record     _
| Remove     _
| Set        _
| Skip       _
| Step       _
| Then       _
| To         _
| Type       _
| Var        _
| While      _
| With       _ -> true
|            _ -> false

let is_constr = function
  Constr  _
| C_False _
| C_None  _
| C_Some  _
| C_True  _
| C_Unit  _ -> true
|         _ -> false

let is_sym = function
  SEMI     _
| COMMA    _
| LPAR     _
| RPAR     _
| LBRACE   _
| RBRACE   _
| LBRACKET _
| RBRACKET _
| CONS     _
| VBAR     _
| ARROW    _
| ASS      _
| EQUAL    _
| COLON    _
| LT       _
| LEQ      _
| GT       _
| GEQ      _
| NEQ      _
| PLUS     _
| MINUS    _
| SLASH    _
| TIMES    _
| DOT      _
| WILD     _
| CAT      _ -> true
|          _ -> false

let is_eof = function EOF _ -> true | _ -> false

(* END TRAILER *)
}
