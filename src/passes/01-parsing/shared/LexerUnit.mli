(* Functor to build a standalone LIGO lexer *)

(* Vendor dependencies *)

module Region = Simple_utils.Region

(* CLI options *)

module type IO =
  sig
    val options : EvalOpt.options
  end

(* The functor itself *)

module Make (IO: IO) (Lexer: Lexer.S) :
  sig
    val scan  : unit -> (Lexer.token list, string Region.reg) Stdlib.result
    val trace : unit -> (unit, string Region.reg) Stdlib.result
  end
