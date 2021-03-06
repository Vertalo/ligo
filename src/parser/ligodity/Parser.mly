%{
(* START HEADER *)

open AST

(* Rewrite "let pattern = e" as "let x = e;; let x1 = ...;; let x2 = ...;;" *)

module VMap = Utils.String.Map

let ghost_of value = Region.{region=ghost; value}
let ghost = Region.ghost

let fail_syn_unif type1 type2 : 'a =
  let reg = AST.region_of_type_expr type1 in
  let reg = reg#compact ~file:false `Byte in
  let value =
    Printf.sprintf "Unification with %s is not\
                    implemented." reg in
  let region = AST.region_of_type_expr type2 in
  let err = Region.{value; region} in
  (Lexer.prerr ~kind:"Syntactical" err; exit 1)

let mk_component rank =
  let num = string_of_int rank, Z.of_int rank in
  let par = {lpar=ghost; inside = ghost_of num; rpar=ghost}
  in Component (ghost_of par)

let rec mk_field_path (rank, tail) =
  let head = mk_component rank in
  match tail with
        [] -> head, []
  | hd::tl -> mk_field_path (hd,tl) |> Utils.nsepseq_cons head ghost

let mk_projection fresh (path : int Utils.nseq) = {
  struct_name = fresh;
  selector    = ghost;
  field_path  = Utils.nsepseq_rev (mk_field_path path)
}

let rec sub_rec fresh path (map, rank) pattern =
  let path' = Utils.nseq_cons rank path in
  let map'  = split fresh map path' pattern
  in map', rank+1

and split fresh map path = function
  PTuple t -> let apply = sub_rec fresh path in
             Utils.nsepseq_foldl apply (map,1) t.value |> fst
| PPar p   -> split fresh map path p.value.inside
| PVar v   -> if VMap.mem v.value map
             then
               let err =
                 Region.{value="Non-linear pattern."; region=v.region}
               in (Lexer.prerr ~kind:"Syntactical" err; exit 1)
             else
               let proj = mk_projection fresh path
               in VMap.add v.value (None, proj) map
| PWild _  -> map
| PUnit _  -> let anon = Utils.gen_sym () in
             let unit = ghost, TAlias (ghost_of "unit")
             and proj = mk_projection fresh path
             in VMap.add anon (Some unit, proj) map
| PRecord {region; _}
| PConstr {region; _}
| PTyped {region; _} ->
    let err = Region.{value="Not implemented yet."; region}
    in (Lexer.prerr ~kind:"Syntactical" err; exit 1)
| p -> let _, _, map = split_pattern p in map

and split_pattern = function
  PPar p  -> split_pattern p.value.inside
| PVar v  -> v, None, VMap.empty
| PWild _ -> Utils.gen_sym () |> ghost_of, None, VMap.empty
| PUnit _ -> let fresh = Utils.gen_sym () |> ghost_of in
            let unit = TAlias (ghost_of "unit")
            in fresh, Some unit, VMap.empty
| PTyped {value=p; _} ->
    let var', type', map = split_pattern p.pattern in
    (match type' with
       None -> var', Some p.type_expr, map
     | Some t when t = p.type_expr -> var', Some t, map (* hack *)
     | Some t -> fail_syn_unif t p.type_expr)
| PTuple t ->
    let fresh = Utils.gen_sym () |> ghost_of
    and init = VMap.empty, 1 in
    let apply (map, rank) pattern =
      split fresh map (rank,[]) pattern, rank+1 in
    let map = Utils.nsepseq_foldl apply init t.value |> fst
    in fresh, None, map
| PRecord {region; _}
| PConstr {region; _} ->
    let err = Region.{value="Not implemented yet."; region}
    in (Lexer.prerr ~kind:"Syntactical" err; exit 1)
| PInt {region; _} | PTrue region
| PFalse region | PString {region; _}
| PList Sugar {region; _} | PList PCons {region; _} ->
    let err = Region.{value="Incomplete pattern."; region}
    in (Lexer.prerr ~kind:"Syntactical" err; exit 1)

let mk_let_bindings =
  let apply var (lhs_type, proj) =
    let new_bind = {
      variable = ghost_of var;
      lhs_type;
      eq = ghost;
      let_rhs = EProj (ghost_of proj)} in
    let new_let = Let (ghost_of (ghost, new_bind))
    in Utils.nseq_cons new_let
  in VMap.fold apply

let mk_let_in_bindings =
  let apply var (lhs_type, proj) acc =
    let binding = {
      variable = ghost_of var;
      lhs_type;
      eq = ghost;
      let_rhs = EProj (ghost_of proj)} in
    let let_in = {
      kwd_let = ghost;
      binding;
      kwd_in = ghost;
      body = acc}
    in ELetIn (ghost_of let_in)
  in VMap.fold apply

(* We rewrite "fun p -> e" into "fun x -> match x with p -> e" *)

let norm_fun_expr patterns expr =
  let apply pattern expr =
    match pattern with
      PVar var ->
        let fun_expr = {
          kwd_fun = ghost;
          param   = var;
          p_annot = None;
          arrow   = ghost;
          body    = expr}
        in EFun (ghost_of fun_expr)
    | PTyped p ->
        let pattern = p.value.pattern
        and type_expr = p.value.type_expr in
        let fresh = Utils.gen_sym () |> ghost_of in
        let clause = {pattern; arrow=ghost; rhs=expr} in
        let clause = ghost_of clause in
        let cases = ghost_of (clause, []) in
        let case = {
          kwd_match = ghost;
          expr      = EVar fresh;
          opening   = With ghost;
          lead_vbar = None;
          cases;
          closing   = End ghost} in
        let case = ECase (ghost_of case) in
        let fun_expr = {
          kwd_fun = ghost;
          param   = fresh;
          p_annot = Some (p.value.colon, type_expr);
          arrow   = ghost;
          body    = case}
        in EFun (ghost_of fun_expr)
    | _ -> let fresh = Utils.gen_sym () |> ghost_of in
          let clause = {pattern; arrow=ghost; rhs=expr} in
          let clause = ghost_of clause in
          let cases = ghost_of (clause, []) in
          let case = {
            kwd_match = ghost;
            expr      = EVar fresh;
            opening   = With ghost;
            lead_vbar = None;
            cases;
            closing   = End ghost} in
          let case = ECase (ghost_of case) in
          let fun_expr = {
            kwd_fun = ghost;
            param   = fresh;
            p_annot = None;
            arrow   = ghost;
            body    = case}
          in EFun (ghost_of fun_expr)
  in Utils.nseq_foldr apply patterns expr

(* END HEADER *)
%}


(* Entry points *)

%start program
%type <AST.t> program

%%

(* RULES *)

(* This parser leverages Menhir-specific features, in particular
   parametric rules, rule inlining and primitives to get the source
   locations of tokens from the lexer engine generated by ocamllex.

     We define below two rules, [reg] and [oreg]. The former parses
   its argument and returns its synthesised value together with its
   region in the source code (that is, start and end positions --- see
   module [Region]). The latter discards the value and only returns
   the region: this is mostly useful for parsing keywords, because
   those can be easily deduced from the AST node and only their source
   region has to be recorded there.
*)

%inline reg(X):
  X { let start  = Pos.from_byte $symbolstartpos
      and stop   = Pos.from_byte $endpos in
      let region = Region.make ~start ~stop
      in Region.{region; value=$1} }

%inline oreg(X):
  reg(X) { $1.Region.region }

(* Keywords, symbols, literals and virtual tokens *)

kwd(X)   : oreg(X)       { $1 }
sym(X)   : oreg(X)       { $1 }
ident    : reg(Ident)    { $1 }
constr   : reg(Constr)   { $1 }
string   : reg(Str)      { $1 }
eof      : oreg(EOF)     { $1 }
vbar     : sym(VBAR)     { $1 }
lpar     : sym(LPAR)     { $1 }
rpar     : sym(RPAR)     { $1 }
lbracket : sym(LBRACKET) { $1 }
rbracket : sym(RBRACKET) { $1 }
lbrace   : sym(LBRACE)   { $1 }
rbrace   : sym(RBRACE)   { $1 }
comma    : sym(COMMA)    { $1 }
semi     : sym(SEMI)     { $1 }
colon    : sym(COLON)    { $1 }
eq       : sym(EQ)       { $1 }
dot      : sym(DOT)      { $1 }
arrow    : sym(ARROW)    { $1 }
wild     : sym(WILD)     { $1 }
cons     : sym(CONS)     { $1 }

(* The rule [sep_or_term(item,sep)] ("separated or terminated list")
   parses a non-empty list of items separated by [sep], and optionally
   terminated by [sep]. *)

sep_or_term_list(item,sep):
  nsepseq(item,sep) {
    $1, None
  }
| nseq(item sep {$1,$2}) {
    let (first,sep), tail = $1 in
    let rec trans (seq, prev_sep as acc) = function
      [] -> acc
    | (item,next_sep)::others ->
        trans ((prev_sep,item)::seq, next_sep) others in
    let list, term = trans ([],sep) tail
    in (first, List.rev list), Some term }

(* Compound constructs *)

par(X): reg(lpar X rpar { {lpar=$1; inside=$2; rpar=$3} }) { $1 }

(* Sequences

   Series of instances of the same syntactical category have often to
   be parsed, like lists of expressions, patterns etc. The simplest of
   all is the possibly empty sequence (series), parsed below by
   [seq]. The non-empty sequence is parsed by [nseq]. Note that the
   latter returns a pair made of the first parsed item (the parameter
   [X]) and the rest of the sequence (possibly empty). This way, the
   OCaml typechecker can keep track of this information along the
   static control-flow graph. The rule [sepseq] parses possibly empty
   sequences of items separated by some token (e.g., a comma), and
   rule [nsepseq] is for non-empty such sequences. See module [Utils]
   for the types corresponding to the semantic actions of those
   rules.
*)

(* Possibly empty sequence of items *)

seq(item):
  (**)           {     [] }
| item seq(item) { $1::$2 }

(* Non-empty sequence of items *)

nseq(item):
  item seq(item) { $1,$2 }

(* Non-empty separated sequence of items *)

nsepseq(item,sep):
  item                       {                        $1, [] }
| item sep nsepseq(item,sep) { let h,t = $3 in $1, ($2,h)::t }

(* Possibly empy separated sequence of items *)

sepseq(item,sep):
  (**)              {    None }
| nsepseq(item,sep) { Some $1 }

(* Helpers *)

type_name   : ident  { $1 }
field_name  : ident  { $1 }
module_name : constr { $1 }
struct_name : ident  { $1 }

(* Non-empty comma-separated values (at least two values) *)

tuple(item):
  item comma nsepseq(item,comma) { let h,t = $3 in $1,($2,h)::t }

(* Possibly empty semicolon-separated values between brackets *)

list_of(item):
  lbracket sepseq(item,semi) rbracket {
   {opening    = LBracket $1;
    elements   = $2;
    terminator = None;
    closing    = RBracket $3} }

(* Main *)

program:
  declarations eof               { {decl = Utils.nseq_rev $1; eof=$2} }

declarations:
  declaration                                                    { $1 }
| declaration declarations { Utils.(nseq_foldl (swap nseq_cons) $2 $1)}

declaration:
  reg(kwd(LetEntry) entry_binding {$1,$2})          { LetEntry $1, [] }
| reg(type_decl)                                    { TypeDecl $1, [] }
| let_declaration                                   {              $1 }

(* Type declarations *)

type_decl:
  kwd(Type) type_name eq type_expr {
    {kwd_type=$1; name=$2; eq=$3; type_expr=$4} }

type_expr:
  cartesian                                              {   TProd $1 }
| reg(sum_type)                                          {    TSum $1 }
| reg(record_type)                                       { TRecord $1 }

cartesian:
  reg(nsepseq(fun_type, sym(TIMES)))                             { $1 }

fun_type:
  core_type                                                 {      $1 }
| reg(arrow_type)                                           { TFun $1 }

arrow_type:
  core_type arrow fun_type                                 { $1,$2,$3 }

core_type:
  type_projection {
    TAlias $1
  }
| reg(reg(core_type) type_constr {$1,$2}) {
    let arg, constr = $1.value in
    let Region.{value=arg_val; _} = arg in
    let lpar, rpar = ghost, ghost in
    let value = {lpar; inside=arg_val,[]; rpar} in
    let arg = {arg with value} in
    TApp Region.{$1 with value = constr, arg}
  }
| reg(type_tuple type_constr {$1,$2}) {
    let arg, constr = $1.value in
    TApp Region.{$1 with value = constr, arg}
  }
| par(cartesian) {
    let Region.{value={inside=prod; _}; _} = $1 in
    TPar {$1 with value={$1.value with inside = TProd prod}} }

type_projection:
  type_name {
    $1
  }
| reg(module_name dot type_name {$1,$2,$3}) {
    let open Region in
    let module_name,_ , type_name = $1.value in
    let value = module_name.value ^ "." ^ type_name.value
    in {$1 with value} }

type_constr:
  type_name { $1                               }
| kwd(Set)  { Region.{value="set";  region=$1} }
| kwd(Map)  { Region.{value="map";  region=$1} }
| kwd(List) { Region.{value="list"; region=$1} }

type_tuple:
  par(tuple(type_expr)) { $1 }

sum_type:
  ioption(vbar) nsepseq(reg(variant),vbar) { $2 }

variant:
  constr kwd(Of) cartesian { {constr=$1; args = Some ($2,$3)} }
| constr                   { {constr=$1; args = None}         }

record_type:
  lbrace sep_or_term_list(reg(field_decl),semi) rbrace {
    let elements, terminator = $2 in {
      opening = LBrace $1;
      elements = Some elements;
      terminator;
      closing = RBrace $3} }

field_decl:
  field_name colon type_expr {
    {field_name=$1; colon=$2; field_type=$3} }

(* Entry points *)

entry_binding:
  ident nseq(sub_irrefutable) type_annotation? eq expr {
    let let_rhs = norm_fun_expr $2 $5 in
    {variable = $1; lhs_type=$3; eq=$4; let_rhs}
  }
| ident type_annotation? eq fun_expr(expr) {
    {variable = $1; lhs_type=$2; eq=$3; let_rhs=$4} }

(* Top-level non-recursive definitions *)

let_declaration:
  reg(kwd(Let) let_binding {$1,$2}) {
    let kwd_let, (binding, map) = $1.value in
    let let0 = Let {$1 with value = kwd_let, binding}
    in mk_let_bindings map (let0,[])
  }

let_binding:
  ident nseq(sub_irrefutable) type_annotation? eq expr {
    let let_rhs = norm_fun_expr $2 $5 in
    let map = VMap.empty in
    {variable=$1; lhs_type=$3; eq=$4; let_rhs}, map
  }
| irrefutable type_annotation? eq expr {
    let variable, type_opt, map = split_pattern $1 in
    match type_opt, $2 with
      Some type1, Some (_,type2) when type1 <> type2 ->
        fail_syn_unif type1 type2
    | Some type1, None ->
         let lhs_type = Some (ghost, type1) in
         {variable; lhs_type; eq=$3; let_rhs=$4}, map
    | _ -> {variable; lhs_type=$2; eq=$3; let_rhs=$4}, map
  }

type_annotation:
  colon type_expr { $1,$2 }

(* Patterns *)

irrefutable:
  reg(tuple(sub_irrefutable))                            {  PTuple $1 }
| sub_irrefutable                                        {         $1 }

sub_irrefutable:
  ident                                                  {    PVar $1 }
| wild                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| reg(record_pattern)                                    { PRecord $1 }
| par(closed_irrefutable)                                {    PPar $1 }

closed_irrefutable:
  irrefutable                                            {         $1 }
| reg(constr_pattern)                                    { PConstr $1 }
| reg(typed_pattern)                                     {  PTyped $1 }

typed_pattern:
  irrefutable colon type_expr  { {pattern=$1; colon=$2; type_expr=$3} }

pattern:
  reg(sub_pattern cons tail {$1,$2,$3})            { PList (PCons $1) }
| reg(tuple(sub_pattern))                          {        PTuple $1 }
| core_pattern                                     {               $1 }

sub_pattern:
  par(tail)                                              {    PPar $1 }
| core_pattern                                           {         $1 }

core_pattern:
  ident                                                  {    PVar $1 }
| wild                                                   {   PWild $1 }
| unit                                                   {   PUnit $1 }
| reg(Int)                                               {    PInt $1 }
| kwd(True)                                              {   PTrue $1 }
| kwd(False)                                             {  PFalse $1 }
| string                                                 { PString $1 }
| par(ptuple)                                            {    PPar $1 }
| reg(list_of(tail))                               { PList (Sugar $1) }
| reg(constr_pattern)                                    { PConstr $1 }
| reg(record_pattern)                                    { PRecord $1 }

record_pattern:
  lbrace sep_or_term_list(reg(field_pattern),semi) rbrace {
    let elements, terminator = $2 in
    {opening = LBrace $1;
     elements = Some elements;
     terminator;
     closing = RBrace $3} }

field_pattern:
  field_name eq sub_pattern {
    {field_name=$1; eq=$2; pattern=$3} }

constr_pattern:
  constr sub_pattern                                   {  $1, Some $2 }
| constr                                               {  $1, None    }

ptuple:
  reg(tuple(tail))                                       {  PTuple $1 }

unit:
  reg(lpar rpar {$1,$2})                                         { $1 }

tail:
  reg(sub_pattern cons tail {$1,$2,$3})            { PList (PCons $1) }
| sub_pattern                                      {               $1 }

(* Expressions *)

expr:
  base_cond__open(expr)                                    {       $1 }
| reg(match_expr(base_cond))                               { ECase $1 }

base_cond__open(x):
  base_expr(x)
| conditional(x)                                                 { $1 }

base_cond:
  base_cond__open(base_cond)                                     { $1 }

base_expr(right_expr):
  let_expr(right_expr)
| fun_expr(right_expr)
| disj_expr_level                                         {        $1 }
| reg(tuple(disj_expr_level))                             { ETuple $1 }

conditional(right_expr):
  reg(if_then_else(right_expr))
| reg(if_then(right_expr))                               {   ECond $1 }

if_then(right_expr):
  kwd(If) expr kwd(Then) right_expr {
    let the_unit = ghost, ghost in
    let ifnot = EUnit {region=ghost; value=the_unit} in
    {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4;
     kwd_else=ghost; ifnot} }

if_then_else(right_expr):
  kwd(If) expr kwd(Then) closed_if kwd(Else) right_expr {
    {kwd_if=$1; test=$2; kwd_then=$3; ifso=$4;
     kwd_else=$5; ifnot = $6} }

base_if_then_else__open(x):
  base_expr(x)                                             {       $1 }
| reg(if_then_else(x))                                     { ECond $1 }

base_if_then_else:
  base_if_then_else__open(base_if_then_else)               {       $1 }

closed_if:
  base_if_then_else__open(closed_if)                       {       $1 }
| reg(match_expr(base_if_then_else))                       { ECase $1 }

match_expr(right_expr):
  kwd(Match) expr kwd(With) vbar? reg(cases(right_expr)) {
    let cases = Utils.nsepseq_rev $5.value in
    {kwd_match = $1; expr = $2; opening = With $3;
     lead_vbar = $4; cases = {$5 with value=cases};
     closing = End ghost}
  }
| kwd(MatchNat) expr kwd(With) vbar? reg(cases(right_expr)) {
    let cases = Utils.nsepseq_rev $5.value in
    let cast = EVar {region=ghost; value="assert_pos"} in
    let cast = ECall {region=ghost; value=cast,($2,[])} in
    {kwd_match = $1; expr = cast; opening = With $3;
     lead_vbar = $4; cases = {$5 with value=cases};
     closing = End ghost} }

cases(right_expr):
  reg(case_clause(right_expr))                       { $1, [] }
| cases(base_cond) vbar reg(case_clause(right_expr)) {
    let h,t = $1 in $3, ($2,h)::t }

case_clause(right_expr):
  pattern arrow right_expr           { {pattern=$1; arrow=$2; rhs=$3} }

let_expr(right_expr):
  reg(kwd(Let) let_binding kwd(In) right_expr {$1,$2,$3,$4}) {
    let kwd_let, (binding, map), kwd_in, body = $1.value in
    let body = mk_let_in_bindings map body in
    let let_in = {kwd_let; binding; kwd_in; body}
    in ELetIn {region=$1.region; value=let_in} }

fun_expr(right_expr):
  kwd(Fun) nseq(irrefutable) arrow right_expr   { norm_fun_expr $2 $4 }

disj_expr_level:
  reg(disj_expr)                          { ELogic (BoolExpr (Or $1)) }
| conj_expr_level                                                { $1 }

bin_op(arg1,op,arg2):
  arg1 op arg2                            { {arg1=$1; op=$2; arg2=$3} }

un_op(op,arg):
  op arg                                            { {op=$1; arg=$2} }

disj_expr:
  bin_op(disj_expr_level, sym(BOOL_OR), conj_expr_level)
| bin_op(disj_expr_level, kwd(Or),      conj_expr_level)         { $1 }

conj_expr_level:
  reg(conj_expr)                         { ELogic (BoolExpr (And $1)) }
| comp_expr_level                        {                         $1 }

conj_expr:
  bin_op(conj_expr_level, sym(BOOL_AND), comp_expr_level)        { $1 }

comp_expr_level:
  reg(lt_expr)                         { ELogic (CompExpr (Lt $1))    }
| reg(le_expr)                         { ELogic (CompExpr (Leq $1))   }
| reg(gt_expr)                         { ELogic (CompExpr (Gt $1))    }
| reg(ge_expr)                         { ELogic (CompExpr (Geq $1))   }
| reg(eq_expr)                         { ELogic (CompExpr (Equal $1)) }
| reg(ne_expr)                         { ELogic (CompExpr (Neq $1))   }
| cat_expr_level                       {                           $1 }

lt_expr:
  bin_op(comp_expr_level, sym(LT), cat_expr_level)  { $1 }

le_expr:
  bin_op(comp_expr_level, sym(LE), cat_expr_level)  { $1 }

gt_expr:
  bin_op(comp_expr_level, sym(GT), cat_expr_level)  { $1 }

ge_expr:
  bin_op(comp_expr_level, sym(GE), cat_expr_level)  { $1 }

eq_expr:
  bin_op(comp_expr_level, eq, cat_expr_level)  { $1 }

ne_expr:
  bin_op(comp_expr_level, sym(NE), cat_expr_level) { $1 }

cat_expr_level:
  reg(cat_expr)                                   {  EString (Cat $1) }
(*| reg(append_expr)                                { EList (Append $1) } *)
| cons_expr_level                                 {                $1 }

cat_expr:
  bin_op(cons_expr_level, sym(CAT), cat_expr_level)              { $1 }

(*
append_expr:
  cons_expr_level sym(APPEND) cat_expr_level               { $1,$2,$3 }
 *)

cons_expr_level:
  reg(cons_expr)                                    { EList (Cons $1) }
| add_expr_level                                    {              $1 }

cons_expr:
  bin_op(add_expr_level, cons, cons_expr_level)                  { $1 }

add_expr_level:
  reg(plus_expr)                                    { EArith (Add $1) }
| reg(minus_expr)                                   { EArith (Sub $1) }
| mult_expr_level                                   {              $1 }

plus_expr:
  bin_op(add_expr_level, sym(PLUS), mult_expr_level)             { $1 }

minus_expr:
  bin_op(add_expr_level, sym(MINUS), mult_expr_level)            { $1 }

mult_expr_level:
  reg(times_expr)                                 {  EArith (Mult $1) }
| reg(div_expr)                                   {   EArith (Div $1) }
| reg(mod_expr)                                   {   EArith (Mod $1) }
| unary_expr_level                                {                $1 }

times_expr:
  bin_op(mult_expr_level, sym(TIMES), unary_expr_level)          { $1 }

div_expr:
  bin_op(mult_expr_level, sym(SLASH), unary_expr_level)          { $1 }

mod_expr:
  bin_op(mult_expr_level, kwd(Mod), unary_expr_level)            { $1 }

unary_expr_level:
  reg(uminus_expr)                       {            EArith (Neg $1) }
| reg(not_expr)                          { ELogic (BoolExpr (Not $1)) }
| call_expr_level                        {                         $1 }

uminus_expr:
  un_op(sym(MINUS), call_expr_level)                             { $1 }

not_expr:
  un_op(kwd(Not), call_expr_level)                               { $1 }

call_expr_level:
  reg(call_expr)                                         {   ECall $1 }
| reg(constr_expr)                                       { EConstr $1 }
| core_expr                                                      { $1 }

constr_expr:
  constr core_expr?                                           { $1,$2 }

call_expr:
  core_expr nseq(core_expr) { $1,$2 }

core_expr:
  reg(Int)                                          { EArith (Int $1) }
| reg(Mtz)                                          { EArith (Mtz $1) }
| reg(Nat)                                          { EArith (Nat $1) }
| ident | reg(module_field)                                 { EVar $1 }
| reg(projection)                                          { EProj $1 }
| string                                        { EString (String $1) }
| unit                                                     { EUnit $1 }
| kwd(False)                          {  ELogic (BoolExpr (False $1)) }
| kwd(True)                           {  ELogic (BoolExpr (True $1))  }
| reg(list_of(expr))                                { EList (List $1) }
| par(expr)                                              {    EPar $1 }
| reg(sequence)                                          {    ESeq $1 }
| reg(record_expr)                                       { ERecord $1 }
| par(expr colon type_expr {$1,$3}) {
    EAnnot {$1 with value=$1.value.inside} }

module_field:
  module_name dot field_name              { $1.value ^ "." ^ $3.value }

projection:
  struct_name dot nsepseq(selection,dot) {
    {struct_name = $1; selector = $2; field_path = $3}
  }
| reg(module_name dot field_name {$1,$3})
  dot nsepseq(selection,dot) {
    let open Region in
    let module_name, field_name = $1.value in
    let value = module_name.value ^ "." ^ field_name.value in
    let struct_name = {$1 with value} in
    {struct_name; selector = $2; field_path = $3} }

selection:
  field_name    { FieldName $1 }
| par(reg(Int)) { Component $1 }

record_expr:
  lbrace sep_or_term_list(reg(field_assignment),semi) rbrace {
    let elements, terminator = $2 in
    {opening = LBrace $1;
     elements = Some elements;
     terminator;
     closing = RBrace $3} }

field_assignment:
  field_name eq expr {
    {field_name=$1; assignment=$2; field_expr=$3} }

sequence:
  kwd(Begin) sep_or_term_list(expr,semi) kwd(End) {
    let elements, terminator = $2 in
    {opening = Begin $1;
     elements = Some elements;
     terminator;
     closing = End $3} }
