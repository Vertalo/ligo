open Trace

type interpreter_error = Errors.interpreter_error
val eval : ?options:Proto_alpha_utils.Memory_proto_alpha.options -> Ast_typed.program -> (string , interpreter_error) result