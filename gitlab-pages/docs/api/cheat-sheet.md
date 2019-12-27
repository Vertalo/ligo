---
id: cheat-sheet
title: Cheat Sheet
---
<div class="cheatsheet">


<!--DOCUSAURUS_CODE_TABS-->
<!--PascaLIGO-->

|Primitive   	|Example|
|---	|---|
|Strings | `"Tezos"`|
|Characters | `"t"`|
|Integers | `42`, `7`|
|Natural numbers | `42n`, `7n`|
|Unit| `unit`|
|Boolean|<pre><code>const hasDriversLicense: bool = False;<br/>const adult: bool = True;</code></pre> |
|Boolean Logic|<pre><code>(not True) == False == (False and True) == (False or False)</code></pre>|
|Mutez (micro tez)| `42mutez`, `7mutez` |
|Address | `"tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx"`, `"KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD"`|
|Addition |`3 + 4`, `3n + 4n`|
|Multiplication & Division| `3 * 4`, `3n * 4n`, `10 / 5`, `10n / 5n`|
|Modulo| `10 mod 3`|
|Tuples| <pre><code>type name is (string * string);<br/>const winner: name = ("John", "Doe");<br/>const firstName: string = winner.0;<br/>const lastName: string = winner.1;</code></pre>|
|Types|`type age is int`, `type name is string`  |
|Includes|```#include "library.ligo"```|
|Functions (short form)|<pre><code>function add (const a : int ; const b : int) : int is<br/>&nbsp;&nbsp;block { skip } with a + b</code></pre>|
|Functions (long form)|<pre><code>function add (const a : int ; const b : int) : int is<br/>&nbsp;&nbsp;block { <br/>&nbsp;&nbsp;&nbsp;&nbsp;const result: int = a + b;<br/>&nbsp;&nbsp;} with result</code></pre>|
| If Statement | <pre><code>if age < 16 <br/>then fail("Too young to drive."); <br/>else const new_id: int = prev_id + 1;</code></pre>|  
|Options|<pre><code>type middleName is option(string);<br/>const middleName : middleName = Some("Foo");<br/>const middleName : middleName = None;</code></pre>|
|Assignment| ```const age: int = 5;```|
|Assignment on an existing variable <br/></br>*⚠️ This feature is not supported at the top-level scope, you can use it e.g. within functions. Works for Records and Maps as well.*| ```age := 18;```, ```p.age := 21``` |
|Type Annotations| ```("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)```|
|Variants|<pre><code>type action is<br/>&#124; Increment of int<br/>&#124; Decrement of int</code></pre>|
|Variant *(pattern)* matching|<pre><code>const a: action = Increment(5);<br/>case a of<br/>&#124; Increment(n) -> n + 1<br/>&#124; Decrement(n) -> n - 1<br/>end</code></pre>|
|Records|<pre><code>type person is record<br/>&nbsp;&nbsp;age: int ;<br/>&nbsp;&nbsp;name: string ;<br/>end<br/><br/>const john : person = record<br/>&nbsp;&nbsp;age = 18;<br/>&nbsp;&nbsp;name = "John Doe";<br/>end<br/><br/>const name: string = john.name;</code></pre>|
|Maps|<pre><code>type prices is map(nat, tez);<br/><br/>const prices : prices = map<br/>&nbsp;&nbsp;10n -> 60mutez;<br/>&nbsp;&nbsp;50n -> 30mutez;<br/>&nbsp;&nbsp;100n -> 10mutez;<br/>end<br/><br/>const price: option(tez) = prices[50n];<br/><br/>prices[200n] := 5mutez;</code></pre>|
|Contracts & Accounts|<pre><code>const destinationAddress : address = "tz1...";<br/>const contract : contract(unit) = get_contract(destinationAddress);</code></pre>|
|Transactions|<pre><code>const payment : operation = transaction(unit, amount, receiver);</code></pre>|
|Exception/Failure|`failwith("Your descriptive error message for the user goes here.")`|

<!--CameLIGO-->

|Primitive   	|Example|
|---	|---|
|Strings | `"Tezos"`|
|Characters | `"t"`|
|Integers | `42`, `7`|
|Natural numbers | `42n`, `7n`|
|Unit| `unit`|
|Boolean|<pre><code>let has_drivers_license: bool = false<br/>let adult: bool = true</code></pre> |
|Boolean Logic|<pre><code>(not true) = false = (false && true) = (false &#124;&#124; false)</code></pre>|
|Mutez (micro tez)| `42mutez`, `7mutez` |
|Address | `("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx": address)`, `("KT1JepfBfMSqkQyf9B1ndvURghGsSB8YCLMD": address)`|
|Addition |`3 + 4`, `3n + 4n`|
|Multiplication & Division| `3 * 4`, `3n * 4n`, `10 / 5`, `10n / 5n`|
|Modulo| `10 mod 3`|
|Tuples| <pre><code>type name = (string * string)<br/>let winner: name = "John", "Doe"<br/>let first_name: string = winner.0<br/>let last_name: string = winner.1</code></pre>|
|Types|`type age = int`, `type name = string`  |
|Includes|```#include "library.mligo"```|
|Functions |<pre><code>let add (a : int) (b : int) : int = a + b </code></pre>|
| If Statement | <pre><code>let new_id: int = if age < 16 <br/> then failwith("Too young to drive.") <br/> else prev_id + 1</code></pre>|
|Options|<pre><code>type middle_name = string option<br/>let middle_name : middle_name = Some "Foo"<br/>let middle_name : middle_name = None</code></pre>|
|Variable Binding | ```let age: int = 5```|
|Type Annotations| ```("tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx" : address)```|
|Variants|<pre><code>type action =<br/>&#124; Increment of int<br/>&#124; Decrement of int</code></pre>|
|Variant *(pattern)* matching|<pre><code>let a: action = Increment 5<br/>match a with<br/>&#124; Increment n -> n + 1<br/>&#124; Decrement n -> n - 1<br/></code></pre>|
|Records|<pre><code>type person = {<br/>&nbsp;&nbsp;age: int ;<br/>&nbsp;&nbsp;name: string ;<br/>}<br/><br/>let john : person = {<br/>&nbsp;&nbsp;age = 18;<br/>&nbsp;&nbsp;name = "John Doe";<br/>}<br/><br/>let name: string = john.name</code></pre>|
|Maps|<pre><code>type prices = (nat, tez) map<br/><br/>let prices : prices = Map.literal [<br/>&nbsp;&nbsp;(10n, 60mutez);<br/>&nbsp;&nbsp;(50n, 30mutez);<br/>&nbsp;&nbsp;(100n, 10mutez)<br/>]<br/><br/>let price: tez option = Map.find 50n prices<br/><br/>let prices: prices = Map.update 200n 5mutez prices</code></pre>|
|Contracts & Accounts|<pre><code>let destination_address : address = "tz1..."<br/>let contract : unit contract = <br/> Operation.get_contract destination_address</code></pre>|
|Transactions|<pre><code>let payment : operation = <br/> Operation.transaction (unit, receiver, amount)</code></pre>|
|Exception/Failure|`failwith("Your descriptive error message for the user goes here.")`|



<!--END_DOCUSAURUS_CODE_TABS-->

</div>