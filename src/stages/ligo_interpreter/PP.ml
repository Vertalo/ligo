open Types

let rec pp_value : value -> string = function
  | V_Ct (C_int i) -> Format.asprintf "%a : int" Z.pp_print i
  | V_Ct (C_nat n) -> Format.asprintf "%a : nat" Z.pp_print n
  | V_Ct (C_string s) -> Format.asprintf "\"%s\" : string" s
  | V_Ct (C_unit) -> Format.asprintf "unit"
  | V_Ct (C_bool true) -> Format.asprintf "true"
  | V_Ct (C_bool false) -> Format.asprintf "false"
  | V_Ct (C_bytes b) -> Format.asprintf "0x%a : bytes" Hex.pp (Hex.of_bytes b)
  | V_Ct (C_mutez i) -> Format.asprintf "%Ld : mutez" (Tez.to_mutez i)
  | V_Ct (C_address s) -> Format.asprintf "\"%s\" : address" s
  | V_Ct (C_timestamp t) -> Format.asprintf "+%a" Z.pp_print t
  | V_Ct (_) -> Format.asprintf "TODO"
  | V_Failure s -> Format.asprintf "\"%s\" : failure " s
  | V_Record recmap ->
    let content = LMap.fold (fun label field prev ->
      let (Label l) = label in
      Format.asprintf "%s ; %s = (%s)" prev l (pp_value field))
      recmap "" in
    Format.asprintf "{ %s }" content
  | V_Func_val _ -> Format.asprintf "<fun>"
  | V_Func_rec _ -> Format.asprintf "<rec fun>"
  | V_Construct (name,v) -> Format.asprintf "%s(%s)" name (pp_value v)
  | V_List vl ->
    Format.asprintf "[%s]" @@
      List.fold_left (fun prev v -> Format.asprintf "%s ; %s" prev (pp_value v)) "" vl
  | V_Map vmap ->
    Format.asprintf "[%s]" @@
      List.fold_left (fun prev (k,v) -> Format.asprintf "%s ; %s -> %s" prev (pp_value k) (pp_value v)) "" vmap
  | V_Set slist ->
    Format.asprintf "{%s}" @@
      List.fold_left (fun prev v -> Format.asprintf "%s ; %s" prev (pp_value v)) "" slist
    
let pp_context : Mini_proto.t -> string = fun { contracts ; step_constants } ->
  let open Mini_proto in
  let {source;payer;self;amount;balance;now;chain_id=_} = step_constants in
  ignore source ;
  ignore payer ;
  ignore self ;
  ignore amount ;
  ignore balance ;
  ignore now ;
  let ct = Mini_proto.StateMap.to_kv_list contracts in
  let ct = List.map
    (fun (Address addr, {script = { code ; storage } ; script_balance }) ->
      Format.asprintf "%s ->\n code : %s \n storage : %s\n script_balance %a\n "
        addr
        (pp_value code)
        (pp_value storage)
        Tez.pp script_balance
    ) ct in
  String.concat "--\n" ("Contracts:\n"::ct)

let pp_env : env -> unit = fun env ->
  let () = Format.printf "{ #elements : %i\n" @@ Env.cardinal env in
  let () = Env.iter (fun var v ->
    Format.printf "\t%a -> %s\n" Var.pp var.wrap_content (pp_value v))
  env in
  let () = Format.printf "\n}\n" in
  ()
