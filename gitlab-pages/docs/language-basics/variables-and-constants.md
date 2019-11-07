---
id: constants-and-variables
title: Constants & Variables
---

The basic building block right after types, are constants and variables.

## Constants

Constants are immutable by design, which means their values can't be reassigned.
When defining a constant, you need to provide a `name`, `type` and a `value`:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const age : int = 25;
```

You can evaluate the constant definition above, using the following CLI command:
```shell
ligo evaluate-value -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.ligo age
# Outputs: 25
```
<!--Cameligo-->
```cameligo
let age: int = 25
```

You can evaluate the constant definition above, using the following CLI command:
```shell
ligo evaluate-value -s cameligo gitlab-pages/docs/language-basics/src/variables-and-constants/const.mligo age
# Outputs: 25
```

<!--END_DOCUSAURUS_CODE_TABS-->

## Variables

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->

Variables, unlike constants are mutable, but can't be used in a *global scope*, however they can be used within functions, or function arguments.

> 💡 Don't worry if you don't understand the function syntax yet, we'll get to it in the upcoming sections of the docs


```pascaligo
// won't work, use const for global values instead
// var four: int = 4;

function add(const a: int; const b: int) : int is
    block { 
        var c : int := a + b;
     } with c
```


> ⚠️ Notice the different assignment operator `:=`

You can run the `add` function defined above using the LIGO compiler like this:

```shell
ligo run-function -s pascaligo gitlab-pages/docs/language-basics/src/variables-and-constants/add.ligo add '(1,1)' 
# Outputs: 2
```

<!--Cameligo-->

As would be expected from a functional language, CameLIGO uses the value-binding
notion of variables rather than assignment. Variables are changed by replacement,
with a new value being bound in place of the old one.

> 💡 Don't worry if you don't understand the function syntax yet, we'll get to it in the upcoming sections of the docs

```cameligo

let add(const a: int; const b: int) : int =
  let c : int = a + b in c
```

You can run the `add` function defined above using the LIGO compiler like this:

```shell
ligo run-function -s cameligo gitlab-pages/docs/language-basics/src/variables-and-constants/add.mligo add '(1,1)' 
# Outputs: 2
```

<!--END_DOCUSAURUS_CODE_TABS-->