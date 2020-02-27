---
id: strings
title: Strings
---

Strings are defined using the built-in `string` type like this:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```
const a : string = "Hello Alice"
```
<!--CameLIGO-->
```
let a : string = "Hello Alice"
```
<!--ReasonLIGO-->
```reasonligo
let a : string = "Hello Alice";
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Concatenating Strings

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
Strings can be concatenated using the `^` operator.

```pascaligo group=a
const name : string = "Alice"
const greeting : string = "Hello"
const full_greeting : string = greeting ^ " " ^ name
```
<!--CameLIGO-->
Strings can be concatenated using the `^` operator.

```cameligo group=a
let name : string = "Alice"
let greeting : string = "Hello"
let full_greeting : string = greeting ^ " " ^ name
```
<!--ReasonLIGO-->
Strings can be concatenated using the `++` operator.

```reasonligo group=a
let name : string = "Alice";
let greeting : string = "Hello";
let full_greeting : string = greeting ++ " " ++ name;
```
<!--END_DOCUSAURUS_CODE_TABS-->


## Extracting Substrings

Substrings can be extracted using the predefined function
`String.sub`. The first character has index 0 and the interval of
indices for the substring has inclusive bounds.

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=b
const name  : string = "Alice"
const slice : string = String.sub (0n, 1n, name)
```

> Note that `string_slice` is *deprecated*. Use `String.sub`.

<!--CameLIGO-->
```cameligo group=b
let name  : string = "Alice"
let slice : string = String.sub 0n 1n name
```

> Note that `String.slice` is *deprecated*. Use `String.sub`.

<!--ReasonLIGO-->
```reasonligo group=b
let name  : string = "Alice";
let slice : string = String.sub (0n, 1n, name);
```

> Note that `String.slice` is *deprecated*. Use `String.sub`.

<!--END_DOCUSAURUS_CODE_TABS-->

> ⚠️ Notice that the offset and length of the slice are natural
> numbers.

## Length of Strings

The length of a string can be found using a built-in function:

<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->
```pascaligo group=c
const name : string = "Alice"
const length : nat = String.length (name) // length = 5
```

> Note that `size` is *deprecated*. Use `String.length`.

<!--CameLIGO-->
```cameligo group=c
let name : string = "Alice"
let length : nat = String.length name  // length = 5
```

> Note that `String.size` is *deprecated*. Use `String.length`.

<!--ReasonLIGO-->
```reasonligo group=c
let name : string = "Alice";
let length : nat = String.length (name);  // length == 5
```

> Note that `String.size` is *deprecated*. Use `String.length`.

<!--END_DOCUSAURUS_CODE_TABS-->
