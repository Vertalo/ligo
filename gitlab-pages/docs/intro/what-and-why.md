---
id: what-and-why
title: What & Why
---

Before we get into what is LIGO and why does LIGO need to exist in the first place, let's take a look at what options does the Tezos blockchain offer us out of the box. If you were to implement smart contracts natively on Tezos, you'd have to learn [Michelson](https://tezos.gitlab.io/whitedoc/michelson.html).

> 💡 The (Michelson) language is stack-based, with high level data types and primitives, and strict static type checking.


Here's an example of Michelson code:

**`counter.tz`**
```text
{ parameter (or (or (nat %add) (nat %sub)) (unit %default)) ;
  storage int ;
  code { AMOUNT ; PUSH mutez 0 ; ASSERT_CMPEQ ; UNPAIR ;
         IF_LEFT
           { IF_LEFT { ADD } { SWAP ; SUB } }
           { DROP ; DROP ; PUSH int 0 } ;
         NIL operation ; PAIR } }
```

The contract above maintains an `int` in its storage. It has two entrypoints *(functions)* `add` and `sub` to modify it, and the default *entrypoint*, of type unit will reset it to 0.

The contract itself contains three main parts:

- `parameter` - Argument that is provided by a transaction invoking the contract
- `storage` - Type definition for the contract's data storage.
- `code` - Actual Michelson code, that has the provided parameter & the current storage value in it's initial stack, and outputs a pair of operations & a new storage value as it's resulting stack.

Michelson code consists of *instructions* like `IF_LEFT`, `PUSH ...`, `UNPAIR` that are bundled togeter in what is called a *sequence*. Stack represents an intermediate state of the program, while **storage represents a persistent state**. Instructions are used to modify the run-time stack in order to yield a desired stack value when the program terminates. 

> 💡 A michelson program ran on the Tezos blockchain is meant to output a pair of values including a `list of operations` to emit and a new `storage` value to persist

## Differences between a stack and traditional variable management

Stack management might be a little bit challanging, especially if you're coming from a *C-like language*. Let's implement a similar program as above, in Javascript:

**`counter.js`**
```javascript
var storage = 0;

function add(a) {
    storage += a
}

function sub(a) {
    storage -= a
}

// We're calling this function reset instead of default
// because `default` is a javascript keyword
function reset() {
    storage = 0;
}
```

In our javascript program, the initial `storage` value is `0`, and it can be modified by running the functions `add(a)`, `sub(a)` and `reset()`.

Unfortunately (???), we **can't run Javascript on the Tezos blockchain** at this moment, however we can opt-in for LIGO, which will abstract the stack management, and allow us to write readable, type-safe and efficient smart contracts.

> 💡 You can try running the javascript program [here](https://codepen.io/maht0rz/pen/dyyvoPQ?editors=0012)

## C-like smart contracts instead of Michelson

Let's take a look at how a similar LIGO program would look like, don't worry if it's a little confusing at first, we'll explain all the syntactical bits in the upcoming sections of the documentation.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
type action is
| Increment of int
| Decrement of int
| Reset of unit

function main (const p : action ; const s : int) : (list(operation) * int) is
 block { skip } with ((nil : list(operation)),
  case p of
  | Increment(n) -> s + n
  | Decrement(n) -> s - n
  | Reset(n) -> 0
 end)
```
<!--END_DOCUSAURUS_CODE_TABS-->



> 💡 You can find the Michelson compilation output of the contract above in **`ligo-counter.tz`**

The LIGO contract behaves exactly* like the Michelson contract we've seen first, and it accepts the following LIGO expressions/values: `Increment(n)`, `Decrement(n)` and `Reset(n)`. Those serve as `entrypoint` identification, same as `%add` `%sub` or `%default` in the Michelson contract.

**not exactly, the Michelson contract also checks if the `AMOUNT` sent is `0`*

---

## Runnable code snippets & exercises

Some of the sections in this documentation will include runnable code snippets and exercises. Sources for those are available at
the [LIGO Gitlab repository](https://gitlab.com/ligolang/ligo). 

### Snippets
For example **code snippets** for the *Types* subsection of this doc, can be found here:
`gitlab-pages/docs/language-basics/src/types/**`

### Exercises
Solutions to exercises can be found e.g. here:  `gitlab-pages/docs/language-basics/exercises/types/**/solutions/**`

### Running snippets / excercise solutions
In certain cases it makes sense to be able to run/evaluate the given snippet or a solution, usually there'll be an example command which you can use, such as:

```shell
ligo evaluate-value -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age
# Outputs: 25
```