// Test a PascaLIGO function which takes another PascaLIGO function as an argument
function foobar (const i : int) : int is
  function foo (const i : int) : int is
    block { skip } with i ;
  function bar (const f : int -> int) : int is
    block { skip } with f ( i ) ;
  block { skip } with bar (foo) ;

// higher order function with more than one argument
function higher2(const i: int; const f: int -> int): int is
    block { 
        const ii: int = f(i)
     } with ii

function foobar2 (const i : int) : int is
  function foo2 (const i : int) : int is
    block { skip } with i;
  block { skip } with higher2(i,foo2)

// This is not supported yet:
// const a : int = 123;
// function foobar3 (const i : int) : int is
//   function foo2 (const i : int) : int is
//     block { skip } with (a+i);
//   block { skip } with higher2(i,foo2)

function f (const i : int) : int is
  block { skip }
  with i

function g (const i : int) : int is
  block { skip }
  with f(i)

function foobar4 (const i : int) : int is
  block { skip }
  with g(g(i))
