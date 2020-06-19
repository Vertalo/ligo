open Types

open Memory_proto_alpha.Protocol.Alpha_context
let assert_operation_eq (a: packed_internal_operation) (b: packed_internal_operation): unit option =
  let Internal_operation {source=sa; operation=oa; nonce=na} = a in
  let Internal_operation {source=sb; operation=ob; nonce=nb} = b in
  let assert_source_eq sa sb =
    let sa = Contract.to_b58check sa in
    let sb = Contract.to_b58check sb in
    if String.equal sa sb then Some () else None
  in
  let rec assert_param_eq (pa,pb) =
    let open Tezos_micheline.Micheline in
    match (pa, pb) with
    | Int (la, ia), Int (lb, ib) when la = lb && ia = ib -> Some ()
    | String (la, sa), String (lb, sb) when la = lb && sa = sb -> Some ()
    | Bytes (la, ba), Bytes (lb, bb) when la = lb && ba = bb -> Some ()
    | Prim (la, pa, nla, aa), Prim (lb, pb, nlb, ab) when la = lb && pa = pb -> 
      let la = List.map assert_param_eq @@ List.combine nla nlb in 
      let lb = List.map ( fun (sa,sb) -> 
        if String.equal sa sb then Some () else None) @@
        List.combine aa ab
      in
      Option.map (fun _ -> ()) @@ Option.bind_list @@ la @ lb 
    | Seq (la, nla), Seq (lb, nlb) when la = lb -> 
      Option.map (fun _ -> ()) @@ Option.bind_list @@ List.map assert_param_eq @@
        List.combine nla nlb
    | _ -> None
  in
  let assert_operation_eq (type a b) (oa: a manager_operation) (ob: b manager_operation) = 
    match (oa, ob) with 
    | Reveal sa, Reveal sb when sa = sb -> Some ()
    | Reveal _, _ -> None
    | Transaction ta, Transaction tb ->
      let aa,pa,ea,da = ta.amount,ta.parameters,ta.entrypoint,ta.destination in
      let ab,pb,eb,db = tb.amount,tb.parameters,tb.entrypoint,tb.destination in
      Format.printf "amount : %b; p : %b, e: %b, d : %b\n" (aa=ab) (pa=pb) (ea=eb) (da=db) ;
      let (pa,pb) = Tezos_data_encoding.Data_encoding.(force_decode pa, force_decode pb) in
      Option.bind (fun _ -> Some ()) @@ 
      Option.bind_list [
        Option.bind (fun (pa,pb) -> assert_param_eq Tezos_micheline.Micheline.(root pa, root pb)) @@
        Option.bind_pair (pa,pb);
      if aa = ab && ea = eb && da = db then Some () else None ]
    | Transaction _, _ -> None
    | Origination _oa, Origination _ob -> Some ()
    | Origination _, _ -> None
    | Delegation da, Delegation db when da = db -> Some ()
    | Delegation _, _ -> None
  in
  let assert_nonce_eq na nb = if na = nb then Some () else None in
  Option.bind (fun _ -> Some ()) @@ 
  Option.bind_list [
    assert_source_eq sa sb; 
    assert_operation_eq oa ob;
    assert_nonce_eq na nb]

let assert_literal_eq (a, b : literal * literal) : unit option =
  match (a, b) with
  | Literal_int a, Literal_int b when a = b -> Some ()
  | Literal_int _, Literal_int _ -> None
  | Literal_int _, _ -> None
  | Literal_nat a, Literal_nat b when a = b -> Some ()
  | Literal_nat _, Literal_nat _ -> None
  | Literal_nat _, _ -> None
  | Literal_timestamp a, Literal_timestamp b when a = b -> Some ()
  | Literal_timestamp _, Literal_timestamp _ -> None
  | Literal_timestamp _, _ -> None
  | Literal_mutez a, Literal_mutez b when a = b -> Some ()
  | Literal_mutez _, Literal_mutez _ -> None
  | Literal_mutez _, _ -> None
  | Literal_string a, Literal_string b when a = b -> Some ()
  | Literal_string _, Literal_string _ -> None
  | Literal_string _, _ -> None
  | Literal_bytes a, Literal_bytes b when a = b -> Some ()
  | Literal_bytes _, Literal_bytes _ -> None
  | Literal_bytes _, _ -> None
  | Literal_void, Literal_void -> Some ()
  | Literal_void, _ -> None
  | Literal_unit, Literal_unit -> Some ()
  | Literal_unit, _ -> None
  | Literal_address a, Literal_address b when a = b -> Some ()
  | Literal_address _, Literal_address _ -> None
  | Literal_address _, _ -> None
  | Literal_operation opa, Literal_operation opb -> assert_operation_eq opa opb
  | Literal_operation _, _ -> None
  | Literal_signature a, Literal_signature b when a = b -> Some ()
  | Literal_signature _, Literal_signature _ -> None
  | Literal_signature _, _ -> None
  | Literal_key a, Literal_key b when a = b -> Some ()
  | Literal_key _, Literal_key _ -> None
  | Literal_key _, _ -> None
  | Literal_key_hash a, Literal_key_hash b when a = b -> Some ()
  | Literal_key_hash _, Literal_key_hash _ -> None
  | Literal_key_hash _, _ -> None
  | Literal_chain_id a, Literal_chain_id b when a = b -> Some ()
  | Literal_chain_id _, Literal_chain_id _ -> None
  | Literal_chain_id _, _ -> None

let rec assert_value_eq (a, b: (expression * expression )) : unit option =
  match (a.expression_content , b.expression_content) with
  | E_literal a , E_literal b ->
    assert_literal_eq (a, b)
  | E_constant (ca) , E_constant (cb) when ca.cons_name = cb.cons_name -> (
      let lst = List.combine ca.arguments cb.arguments in
      let all = List.map assert_value_eq lst in
      if List.exists (Option.is_none) all then None else Some ()
    )
  | E_constructor (ca), E_constructor (cb) when ca.constructor = cb.constructor -> (
      assert_value_eq (ca.element, cb.element)
    )
  | E_record sma, E_record smb -> (
      let aux _ a b =
        match a, b with
        | Some a, Some b -> assert_value_eq (a, b)
        | _ -> None
      in
      let all = LMap.merge aux sma smb in
      if    ((LMap.cardinal all) = (LMap.cardinal sma)) 
         || ((LMap.cardinal all) = (LMap.cardinal smb)) then
        Some ()
      else None
    )
  | E_record_update ura, E_record_update urb -> (
    match assert_value_eq (ura.record, urb.record) with
    | None -> None
    | Some () ->
      let aux (Label a,Label b) =
        assert (String.equal a b)
      in
      let () = aux (ura.path, urb.path) in
      assert_value_eq (ura.update,urb.update)
  )
  | E_record_update _, _ -> None
  | (E_ascription a ,  _b') -> assert_value_eq (a.anno_expr , b)
  | (_a' , E_ascription b) -> assert_value_eq (a , b.anno_expr)

  | (E_variable _, _) | (E_lambda _, _)
  | (E_application _, _) | (E_let_in _, _)
  | (E_raw_code _, _)
  | (E_recursive _,_) | (E_record_accessor _, _)
  | (E_matching _, _)
   -> None

  | E_literal _ , _
  | E_constant _ , E_constant _
  | E_constant _ , _
  | E_constructor _, E_constructor _
  | E_record _, _
  | E_constructor _, _ ->
      None

let is_value_eq (a , b) =
  match assert_value_eq (a , b) with
  | Some () -> true
  | None -> false
