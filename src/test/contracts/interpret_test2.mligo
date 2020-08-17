type storage = int
type return = operation list * storage
type parameter = One | Two

let addr1 = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address)
let addr2 = ("tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc":address)

let main_fail (action, store : parameter * storage) : return =
  (failwith "main fail !" : return)

let main1 (action, store : parameter * storage) : return =
  match action with
    | One ->
      let c : parameter contract option = Tezos.get_contract_opt addr2 in
      let ops = match c with
          Some (c) -> [ Tezos.transaction (One:parameter) 10tez c ]
        | None     -> (failwith ("Contract not found") : operation list)
      in
      (ops, 1)
    | Two -> (([] : operation list), 2) 

let main2 (action, store : parameter * storage) : return =
  ([] : operation list),
    (match action with
      | One -> 4
      | Two -> 5
    )

let main_self (action, store : parameter * (parameter contract option)) : operation list * (parameter contract option) =
  ( ([] : operation list), Some (Tezos.self "%default" : parameter contract) )


let assert_failure =
  let unit_ = Test.inject_script addr1 main_fail 0 in
  Test.assert_failure (fun (toto:unit) -> Test.external_call addr1 (One:parameter) 1tz)

let assert_failure_internal =
  let unit_ = Test.inject_script addr1 main1 0 in
  let unit_ = Test.inject_script addr2 main_fail 0 in
  Test.assert_failure (fun (u:unit) -> Test.external_call addr1 (One:parameter) 1tz)

let test1 =
  let unit_ = Test.inject_script addr1 main1 0 in
  let unit_ = Test.inject_script addr2 main2 0 in

  let unit_ = Test.set_balance addr1 0tz in
  let unit_ = Test.set_balance addr2 0tz in

  let unit_ = Test.set_now Tezos.now in
  let unit_ = Test.set_source addr1 in

  let unit_ = Test.external_call addr1 (One:parameter) 1tz in
  (* TODO : Balance aren't checked *)

  let a : int  = Test.get_storage addr1 in
  let b : int  = Test.get_storage addr2 in
  (a = 1) && (b = 4)

let contract_vs_address =
  let unit_ = Test.inject_script addr2 main2 0 in
  let c : parameter contract option = Tezos.get_contract_opt addr2 in
  let addr = match c with
    | Some (c) -> c
    | None     -> (failwith ("Contract not found") : parameter contract)
  in
  addr

let self =
  let unit_ = Test.inject_script addr1 main_self ( None : parameter contract option) in
  let unit_ = Test.external_call addr1 (One:parameter) 1tz in
  let c : parameter contract option = Test.get_storage addr1 in
  c