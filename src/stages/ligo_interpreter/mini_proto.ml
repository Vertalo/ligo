open Proto_alpha_utils.Memory_proto_alpha.Protocol
open Alpha_context

type addr = Address of string
module StateMap = Map.Make(struct type t = addr let compare (Address a) (Address b) = String.compare a b end)

type script = {
    code :  Types.value ;
    storage : Types.value ;
  }

type state = {
    script : script ;
    script_balance : Tez.t ;
  }
type state_map = state StateMap.t

type step_constants = {
    source : Contract.t ;
    payer : Contract.t ;
    self : Contract.t ;
    amount : Tez.t ;
    balance : Tez.t ;
    chain_id : Environment.Chain_id.t ;
    now : Script_timestamp.t ;
  }

type t = {
    state : state_map ;
    step_constants : step_constants ;
  }

let option_to_context : Proto_alpha_utils.Memory_proto_alpha.options -> t =
  fun {tezos_context=_TODO;source;payer;self;amount;chain_id;balance;now} ->
    { state = StateMap.empty ;
      step_constants = { source ; payer ; self ; amount ; balance ; chain_id ; now } }
