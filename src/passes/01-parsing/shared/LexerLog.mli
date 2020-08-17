(* Embedding the LIGO lexer in a debug module (logger) *)

(* Vendor dependencies *)

module Region   = Simple_utils.Region
module LexerLib = Simple_utils.LexerLib
module Markup   = Simple_utils.Markup

(* Signature of the logger *)

module type S =
  sig
    module Lexer : Lexer.S
    type token = Lexer.token

    val output_token :
      ?offsets:bool ->
      [`Byte | `Point] ->
      EvalOpt.command ->
      out_channel ->
      Markup.t list ->
      token ->
      unit

    type file_path = string

    val trace :
      ?offsets:bool ->
      [`Byte | `Point] ->
      ?block:LexerLib.block_comment ->
      ?line:LexerLib.line_comment ->
      token_to_region:(token -> Region.t) ->
      style:(token ->
             (Lexing.lexbuf -> (Markup.t list * token) option) ->
             Lexing.lexbuf ->
             unit) ->
      LexerLib.input ->
      EvalOpt.command ->
      (unit, string Region.reg) Stdlib.result
  end

module Make (Lexer: Lexer.S) : S with module Lexer = Lexer
