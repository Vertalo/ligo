---
id: strings
title: Strings
---



Strings can be defined using the built-in `string` type like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```
const a: string = "Hello Alice";
```
<!--Cameligo-->
```
let a: string = "Hello Alice"
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Concatenating strings

Strings are concatenated using the `^` operator.

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const name: string = "Alice";
const greeting: string = "Hello";
// Hello Alice
const full_greeting: string = greeting ^ " " ^ name;
```
<!--Cameligo-->
```cameligo
let name: string = "Alice"
let greeting: string = "Hello"
let full_greeting: string = greeting ^ " " ^ name
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Slicing strings

Strings can be sliced using the syntax specific built-in built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const name: string = "Alice";
// slice = "A"
const slice: string = string_slice(0n, 1n, name);
```
<!--Cameligo-->
```cameligo
let name: string = "Alice"
let slice: string = String.slice 0n 1n name
```
<!--END_DOCUSAURUS_CODE_TABS-->

> ⚠️ Notice that the `offset` and slice `length` are `nats`

## Aquiring a length of a string

Length of a string can be found using the syntax specific built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--Pascaligo-->
```pascaligo
const name: string = "Alice";
// length = 5
const length: nat = size(name);
```
<!--Cameligo-->
```cameligo
let name: string = "Alice"
let length: nat = String.size name
```
<!--END_DOCUSAURUS_CODE_TABS-->