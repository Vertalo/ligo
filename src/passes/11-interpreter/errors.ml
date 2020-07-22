
type interpreter_error = [
  | `Ligo_interpret_overflow of Location.t
  | `Ligo_interpret_reject of Location.t * Ligo_interpreter.Types.value
  | `TODO
]
