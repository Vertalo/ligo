open Types

let pp_ct : Format.formatter -> constant_val -> unit = fun ppf c ->
  match c with
  | C_unit -> Format.fprintf ppf "unit"
  | C_bool t -> Format.fprintf ppf "%b : bool" t
  | C_int z -> Format.fprintf ppf "%s : int" (Int.to_string z)
  | C_nat n -> Format.fprintf ppf "%s : nat" (Int.to_string n)
  | C_timestamp t -> Format.fprintf ppf "%a : timestamp" Z.pp_print t
  | C_mutez m -> Format.fprintf ppf "%Ld : mutez" (Tez.to_mutez m)
  | C_string s -> Format.fprintf ppf "\"%s\" : string" s
  | C_bytes b -> Format.fprintf ppf "0x%a : bytes" Hex.pp (Hex.of_bytes b)
  | C_address s -> Format.fprintf ppf "%s : address" s
  | C_signature s -> Format.fprintf ppf "%s : signature" s
  | C_key s -> Format.fprintf ppf "%s : key" s
  | C_key_hash s -> Format.fprintf ppf "%s : key_hash" s
  | C_chain_id s -> Format.fprintf ppf "%s : chain_id" s
  | C_operation _ -> Format.fprintf ppf "_ : operation"

let rec pp_value : value -> string = function
  | V_Ct c -> Format.asprintf "%a" pp_ct c
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
