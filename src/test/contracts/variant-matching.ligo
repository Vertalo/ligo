type foobar is
| Foo of int
| Bar of bool
| Kee of nat

function fb(const p : foobar) : int is
  block { skip } with (case p of
  | Foo (n) -> n
  | Bar (t) -> 42
  | Kee (n) -> 23
  end)
