(* This Menhir specification defines a parser for the Roman numerals
   up to 999, in the context of the DIP extensible macro. *)

%{
(* START HEADER *)
(* END HEADER *)
%}

(* Tokens *)

%token I V X L C D M
%token P

(* Entry points *)

%start macro
%type <int> macro

%%

(* Rules *)

macro:
  D numeral P         { $2 }

numeral:
                units { $1       }
|          tens       { $1       }
|          tens units { $1+$2    }
| hundreds            { $1       }
| hundreds      units { $1+$2    }
| hundreds tens       { $1+$2    }
| hundreds tens units { $1+$2+$3 }

hundreds:
  c     { $1       }
| C D   { 400      }
| D     { 500      }
| D c   { 500 + $2 }
| C M   { 900      }

c:
  C     { 100      }
| C C   { 200      }
| C C C { 300      }

tens:
  x     { $1       }
| X L   { 40       }
| L     { 50       }
| L x   { 50 + $2  }
| X C   { 90       }

x:
  X     { 10       }
| X X   { 20       }
| X X X { 30       }

units:
  i     { $1       }
| I V   { 4        }
| V     { 5        }
| V i   { 5 + $2   }
| I X   { 9        }

i:
  I     { 1        }
| I I   { 2        }
| I I I { 3        }
