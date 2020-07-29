open Proto_alpha_utils.Memory_proto_alpha
open Protocol
open Trace

module LT = Ligo_interpreter.Types
module Mini_proto = Ligo_interpreter.Mini_proto
module Int_repr = Ligo_interpreter.Int_repr_copied

(* type context = Proto_alpha_utils.Memory_proto_alpha.options *)
type context = Ligo_interpreter.Mini_proto.t
type execution_trace = unit
type 'a result_monad = ('a,Errors.interpreter_error) result


module Command = struct
  type 'a t =
    | Fail_overflow : Location.t -> 'a t
    | Fail_reject : Location.t * LT.value -> 'a t
    | Parse_contract_for_script : Alpha_context.Contract.t * string -> unit t
    | Now : Z.t t
    | Amount : LT.Tez.t t
    | Balance : LT.Tez.t t
    | Sender : string t
    | Source : string t
    | Serialize_pack_data : 'a -> 'a t
    | Serialize_unpack_data : 'a -> 'a t
    | Tez_compare_wrapped : LT.Tez.t * LT.Tez.t -> int t
    | Int_compare_wrapped : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_compare : 'a Int_repr.num * 'a Int_repr.num -> int t
    | Int_abs : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_of_zint : Z.t -> Int_repr.z Int_repr.num t
    | Int_to_zint : 'a Int_repr.num -> Z.t t
    | Int_of_int64 : int64 -> Int_repr.z Int_repr.num t
    | Int_to_int64 : _ Int_repr.num -> int64 option t
    | Int_is_nat : Int_repr.z Int_repr.num -> Int_repr.n Int_repr.num option t
    | Lift_tz_result : 'a Memory_proto_alpha.Alpha_environment.Error_monad.tzresult -> 'a t
    | Int_neg : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_add_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_mul : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_mul_n : Int_repr.n Int_repr.num * Int_repr.n Int_repr.num -> Int_repr.n Int_repr.num t
    | Int_ediv :
      _ Int_repr.num * _ Int_repr.num ->
      (Int_repr.z Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_ediv_n :
      Int_repr.n Int_repr.num * Int_repr.n Int_repr.num ->
      (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) option t
    | Int_sub : _ Int_repr.num * _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_shift_left : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_shift_right : 'a Int_repr.num * Int_repr.n Int_repr.num -> 'a Int_repr.num option t
    | Int_logor : ('a Int_repr.num * 'a Int_repr.num) -> 'a Int_repr.num t
    | Int_logand : (_ Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_logxor : (Int_repr.n Int_repr.num * Int_repr.n Int_repr.num) -> Int_repr.n Int_repr.num t
    | Int_lognot : _ Int_repr.num -> Int_repr.z Int_repr.num t
    | Int_of_int : int -> Int_repr.z Int_repr.num t
    | Int_int : Int_repr.n Int_repr.num -> Int_repr.z Int_repr.num t

  let eval
    : type a.
      a t ->
      context ->
      execution_trace ref option ->
      (a * context) result_monad
    = fun command ctxt _log ->
    (* let get_log (log : execution_trace ref option) =
      match log with
      | Some x -> Some !x
      | None -> None in *)
    match command with
    | Fail_overflow location ->
      fail (`Ligo_interpret_overflow location)
    | Fail_reject (location, e) ->
      fail (`Ligo_interpret_reject (location,e))
    | Now -> ok (LT.Timestamp.to_zint ctxt.step_constants.now, ctxt)
    | Amount -> ok (ctxt.step_constants.amount, ctxt)
    | Balance -> ok (ctxt.step_constants.balance, ctxt)
    | Sender -> ok (Alpha_context.Contract.to_b58check ctxt.step_constants.payer, ctxt)
    | Source -> ok (Alpha_context.Contract.to_b58check ctxt.step_constants.source, ctxt)
    | Serialize_pack_data v -> ok (v,ctxt)
    | Serialize_unpack_data v -> ok (v,ctxt)
    | Parse_contract_for_script _ -> Trace.fail `TODO
    | Tez_compare_wrapped (x, y) ->
      ok (Memory_proto_alpha.Protocol.Script_ir_translator.wrap_compare LT.Tez.compare x y, ctxt)
    | Int_compare_wrapped (x, y) ->
      ok (Memory_proto_alpha.Protocol.Script_ir_translator.wrap_compare Int_repr.compare x y, ctxt)
    | Int_compare (x, y) -> ok (Int_repr.compare x y, ctxt)
    | Int_abs z -> ok (Int_repr.abs z, ctxt)
    | Int_of_int i -> ok (Int_repr.of_int i, ctxt)
    | Int_of_zint z -> ok (Int_repr.of_zint z, ctxt)
    | Int_to_zint z -> ok (Int_repr.to_zint z, ctxt)
    | Int_of_int64 i -> ok (Int_repr.of_int64 i, ctxt)
    | Int_to_int64 i -> ok (Int_repr.to_int64 i, ctxt)
    | Int_is_nat z -> ok (Int_repr.is_nat z, ctxt)
    | Int_neg n -> ok (Int_repr.neg n, ctxt)
    | Int_add (x, y) -> ok (Int_repr.add x y, ctxt)
    | Int_add_n (x, y) -> ok (Int_repr.add_n x y, ctxt)
    | Int_mul (x, y) -> ok (Int_repr.mul x y, ctxt)
    | Int_mul_n (x, y) -> ok (Int_repr.mul_n x y, ctxt)
    | Int_ediv (x, y) -> ok (Int_repr.ediv x y, ctxt)
    | Int_ediv_n (x, y) -> ok (Int_repr.ediv_n x y, ctxt)
    | Int_sub (x, y) -> ok (Int_repr.sub x y, ctxt)
    | Int_shift_left (x, y) -> ok (Int_repr.shift_left x y, ctxt)
    | Int_shift_right (x, y) -> ok (Int_repr.shift_right x y, ctxt)
    | Int_logor (x, y) -> ok (Int_repr.logor x y, ctxt)
    | Int_logand (x, y) -> ok (Int_repr.logand x y, ctxt)
    | Int_logxor (x, y) -> ok (Int_repr.logxor x y, ctxt)
    | Int_lognot n -> ok (Int_repr.lognot n, ctxt)
    | Int_int n -> ok (Int_repr.int n, ctxt)
    | Lift_tz_result r ->
      let%bind r = Proto_alpha_utils.Trace.trace_alpha_tzresult (fun _ -> `TODO) r in
      ok (r, ctxt)
end

type 'a t =
  | Bind : 'a t * ('a -> 'b t) -> 'b t
  | Call : 'a Command.t -> 'a t
  (* | Gas_set_unlimited : 'a t -> 'a t *)
  | Return : 'a -> 'a t
  (* | Trace : Error.t * 'a t -> 'a t *)
  | Bind_err : 'a result_monad -> 'a t

let rec eval
  : type a.
    a t ->
    context ->
    execution_trace ref option ->
    (a * context) result_monad
  = fun e ctxt log ->
  match e with
  | Bind (e', f) ->
    let%bind (v, ctxt) = eval e' ctxt log in
    eval (f v) ctxt log
  | Call command -> Command.eval command ctxt log
  | Return v -> ok (v, ctxt)
  | Bind_err x ->
    let%bind x = x in
    ok (x, ctxt)
  (* | Trace (error, e') -> trace (Script_interpreter_error error) (eval e' ctxt log) *)

(* module Let_syntax = struct
  let bind m ~f = Bind (m, f)
  module Open_on_rhs_bind = struct end
end *)

let return (x: 'a) : 'a t = Return x
let call (command : 'a Command.t) : 'a t = Call command
let ( let>> ) o f = Bind (call o, f)
let ( let* ) o f = Bind (o, f)
let bind_err (x: 'a result_monad) : 'a t = Bind_err x

let rec bind_list = function
  | [] -> return []
  | hd::tl ->
    let* hd = hd in
    let* tl = bind_list tl in
    return @@ hd :: tl

let bind_map_list f lst = bind_list (List.map f lst)

let bind_fold_list f init lst =
  let aux x y = let* x = x in f x y
  in List.fold_left aux (return init) lst