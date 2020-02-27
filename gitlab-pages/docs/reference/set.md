---
id: set-reference
title: Set
---

*Sets* are unordered collections of values of the same type, like
lists are ordered collections. Like the mathematical sets and lists,
sets can be empty and, if not, elements of sets in LIGO are *unique*,
whereas they can be repeated in a *list*.

## Declaring a Set

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

In PascaLIGO, the type of a set of values of type `t` is `set (t)`.
```pascaligo group=set
type integers is set (int)
```

<!--CameLIGO-->

In CameLIGO, the type of a set of values of type `t` is `t set`.

```cameligo group=set
type integers = int set
```

<!--ReasonLIGO-->

In ReasonLIGO, the type of a set of values of type `t` is `set (int)`.

```reasonligo group=set
type integers = set (int);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Creating an Empty Set

Empty sets need a type annotation, either as part of a declaration of
an expression.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->

In PascaLIGO, the notation for sets is similar to that for lists,
except the keyword `set` is used instead of `list`:

```pascaligo group=set
const my_set : set (int) = set []
```
<!--CameLIGO-->

In CameLIGO, the empty set is denoted by the predefined value
`Set.empty`.

```cameligo group=set
let my_set : int set = Set.empty
```

<!--ReasonLIGO-->

In ReasonLIGO, the empty set is denoted by the predefined value
`Set.empty`.

```reasonligo group=set
let my_set : set (int) = Set.empty;
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Creating a Non-Empty Set

A non-empty set can be created by giving all its elements. Remember
that if an element is repeated, only one copy will be retained in the
set.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=set
const my_set : set (int) = set [3; 2; 2; 1]
```

<!--CameLIGO-->
```cameligo group=set
let my_set : int set =
  Set.add 3 (Set.add 2 (Set.add 2 (Set.add 1 (Set.empty : int set))))
```

<!--ReasonLIGO-->
```reasonligo group=set
let my_set : set (int) =
  Set.add (3, Set.add (2, Set.add (2, Set.add (1, Set.empty : set (int)))));
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Set Membership

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=set
const contains_3 : bool = my_set contains 3
```

<!--CameLIGO-->
```cameligo group=set
let contains_3 : bool = Set.mem 3 my_set
```

<!--ReasonLIGO-->
```reasonligo group=set
let contains_3 : bool = Set.mem (3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Cardinal of a Set

The predefined function `Set.cardinal` returns the number of
elements in a given set as follows.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=set
const cardinal : nat = Set.cardinal (my_set)
```

> Note that `size` is *deprecated*. Use `Set.cardinal`.

<!--CameLIGO-->
```cameligo group=set
let cardinal : nat = Set.cardinal my_set
```

> Note that `Set.size` is *deprecated*. Use `Set.cardinal`.

<!--ReasonLIGO-->
```reasonligo group=set
let cardinal : nat = Set.cardinal (my_set);
```

> Note that `Set.size` is *deprecated*. Use `Set.cardinal`.

<!--END_DOCUSAURUS_CODE_TABS-->

## Updating a Set

There are two ways to update a set, that is to add or remove from it.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
In PascaLIGO, either we create a new set from the given one, or we
modify it in-place. First, let us consider the former way:

```pascaligo group=set
const larger_set  : set (int) = Set.add (4, my_set)
const smaller_set : set (int) = Set.remove (3, my_set)
```

> Note that `set_add` and `set_remove` are *deprecated*. Use `Set.add`
> and `Set.remove`.

If we are in a block, we can use an instruction to modify the set
bound to a given variable. This is called a *patch*. It is only
possible to add elements by means of a patch, not remove any: it is
the union of two sets.

```pascaligo group=set
function update (var s : set (int)) : set (int) is block {
  patch s with set [4; 7]
} with s

const new_set : set (int) = update (my_set)
```

<!--CameLIGO-->
```cameligo group=set
let larger_set  : int set = Set.add 4 my_set
let smaller_set : int set = Set.remove 3 my_set
```

<!--ReasonLIGO-->
```reasonligo group=set
let larger_set  : set (int) = Set.add (4, my_set);
let smaller_set : set (int) = Set.remove (3, my_set);
```
<!--END_DOCUSAURUS_CODE_TABS-->

## Functional Iteration over Sets

A *functional iterator* is a function that traverses a data structure
and calls in turn a given function over the elements of that structure
to compute some value. Another approach is possible in PascaLIGO:
*loops* (see the relevant section).

There are three kinds of functional iterations over LIGO maps: the
*iterated operation*, the *mapped operation* (not to be confused with
the *map data structure*) and the *folded operation*.

### Iterated Operation

The first, the *iterated operation*, is an iteration over the map with
no return value: its only use is to produce side-effects. This can be
useful if for example you would like to check that each value inside
of a map is within a certain range, and fail with an error otherwise.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=set
function iter_op (const s : set (int)) : unit is
  block {
    function iterated (const i : int) : unit is
      if i > 2 then Unit else (failwith ("Below range.") : unit)
  } with Set.iter (iterated, s)
```

> Note that `set_iter` is *deprecated*. Use `Set.iter`.

<!--CameLIGO-->
```cameligo group=set
let iter_op (s : int set) : unit =
  let predicate = fun (i : int) -> assert (i > 3)
  in Set.iter predicate s
```

<!--ReasonLIGO-->
```reasonligo group=set
let iter_op = (s : set (int)) : unit => {
  let predicate = (i : int) => assert (i > 3);
  Set.iter (predicate, s);
};
```
<!--END_DOCUSAURUS_CODE_TABS-->

### Folded Operation

A *folded operation* is the most general of iterations. The folded
function takes two arguments: an *accumulator* and the structure
*element* at hand, with which it then produces a new accumulator. This
enables having a partial result that becomes complete when the
traversal of the data structure is over.

<!--DOCUSAURUS_CODE_TABS-->

<!--PascaLIGO-->
```pascaligo group=set
function sum (const acc : int; const i : int): int is acc + i
const sum_of_elements : int = Set.fold (sum, my_set, 0)
```

> Note that `set_fold` is *deprecated*. Use `Set.fold`.

It is possible to use a *loop* over a set as well.

```pascaligo group=set
function loop (const s : set (int)) : int is block {
  var sum : int := 0;
  for element in set s block {
    sum := sum + element
  }
} with sum
```

<!--CameLIGO-->
```cameligo group=set
let sum (acc, i : int * int) : int = acc + i
let sum_of_elements : int = Set.fold sum my_set 0
```

<!--ReasonLIGO-->
```reasonligo group=set
let sum = ((acc, i) : (int, int)) : int => acc + i;
let sum_of_elements : int = Set.fold (sum, my_set, 0);
```
<!--END_DOCUSAURUS_CODE_TABS-->
