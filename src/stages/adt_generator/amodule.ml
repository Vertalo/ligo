type root =
| A of rootA
| B of rootB
| C of string

and a = {
  a1 : ta1 ;
  a2 : ta2 ;
}

and ta1 =
| X of root
| Y of ta2

and ta2 =
| Z of ta2
| W of unit

and rootA = a list

and rootB = int list
