/* Test that the string concatenation syntax in Reasonligo works */

let size_op = (s: string): nat => String.size(s);

let slice_op = (s: string): string => String.slice(1n, 2n, s);

let concat_syntax = (s: string) => s ++ "test_literal";