type param is
| Zero of nat
| Pos of nat

function main (const p : param; const s : unit) : list(operation) * unit is
  block {
    case p of
    | Zero (n) -> if n > 0n then failwith("fail") else skip
    | Pos (n) -> if n > 0n then skip else failwith("fail")
    end
  }
  with ((nil : list(operation)), s)

function foobar (const i : int) : int is
  block {
    var p : param := Zero (42n) ;
    if i > 0 then block {
      i := i + 1 ;
      if i > 10 then block {
        i := 20 ;
        failwith (("who", "knows")) ;
        i := 30 ;
      } else skip
    } else block {
      case p of
      | Zero (n) -> failwith(123n)
      | Pos (n) -> skip
      end
    }
  } with case p of
  | Zero (n) -> i
  | Pos (n) -> (failwith(Zero(0n)) : int)
  end
