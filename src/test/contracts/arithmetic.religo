/* Test CameLIGO arithmetic operators */

let mod_op = (n: int): nat => n mod 42;

let plus_op = (n: int): int => n + 42;

let minus_op = (n: int): int => n - 42;

let times_op = (n: int): int => n * 42;

let div_op = (n: int): int => n / 2;

/* TODO (?): Support conversion from nat to int and back

   let int_op (n : nat) : int =
     Int n

   */

let neg_op = (n: int): int => - n;

let foo = (n: int): int => n + 10;

let neg_op_2 = (b: int): int => - foo(b);