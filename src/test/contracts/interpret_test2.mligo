type storage = int
type return = operation list * storage
type parameter =
  | One
  | Two

let addr1 = ("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx":address)
let addr2 = ("tz2VGBaXuS6rnaa5hpC92qkgadRJKdEbeGwc":address)

let main1 (action, store : parameter * storage) : return =
    (match action with
      | One ->
        // let c : parameter contract option = Tezos.get_contract_opt addr2 in
        // let op = match c with
        //     Some (c) -> [ Tezos.transaction (One:parameter) 10tez c ]
        //   | None     -> (failwith ("Contract not found") : operation list)
        // in
        // (op, 1)
        (([] : operation list),1)
      | Two -> (([] : operation list),2) 
    )
  (*
    here, if Tezos.transaction
    t.step_constants.source <- t.step_constants.self
  *)

let main2 (action, store : parameter * storage) : return =
  ([] : operation list),
    (match action with
      | One -> 4
      | Two -> 5
    )

(*
*)
let test =
  let unit_ = Test.inject_script addr1 main1 0 in
  let unit_ = Test.inject_script addr2 main2 0 in

  let unit_ = Test.set_balance addr1 0tz in
  let unit_ = Test.set_balance addr2 0tz in

  let unit_ = Test.set_now Tezos.now in
  let unit_ = Test.set_source addr1 in

  let ops = Test.external_call addr1 (One:parameter) 1tz in
  let ops = Test.external_call addr1 (Two:parameter) 1tz in

  let a : int  = Test.get_storage addr1 in
  (a = 2)

(* 
let test =
  let () = Test.set_now "14:36:12" in
  let () = Test.set_source "ADDR-X"

  let () = Test.inject_script "ADDR-1" main1 0 in
  let () = Test.set_balance "ADDR-1" 1tz

  let () = Test.external_call (One:parameter) 10mutez "ADDR-1" in

  let () = Test.inject_script "ADDR-2" main2 10 in
*)

    (*
      let script = map.find (Address "ADDR-1") t.state in

      t.step_constants.payer <- t.step_constants.source
      t.step_constants.self <- "ADDR-1"
      t.step_constants.amount <- 10mutez
      t.step_constants.balance <- script.script_balance

      let app = E_application {lamb = script.code ; args = PAIR( script.storage, One)}
      eval toto env
    *)
  