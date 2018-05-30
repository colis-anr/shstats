
(*

grammar of test expressions:

<S>        -> <disj> EOF
<disj>     -> <conj> | <conj> -o <disj>
<conj>     -> <literal> | <literal> -a <conj>
<literal>  -> <atom> | ! <atom>
<atom>     -> string | unop string | string binop string | ( <disj> ) 

grammar in LL(1):

<S>        -> <disj> EOF
<disj>     -> <conj> <disj'>
<disj'>    -> EPSILON | -o <disj>
<conj>     -> <literal> <conj'>
<conj'>    -> EPSILON | -a <conj>
<literal>  -> <atom> | ! <atom>
<atom>     -> string <atom'> | unop string | ( <disj> ) 
<atom'>    -> EPSILON | binop string

annulating non-terminals: { <disj'>, <conj'>, <atom'> } 

nonterminal | Fi_1
------------+--------------------
<S>         | string, unop, (, !
<disj>      | string, unop, (, !
<disj'>     | -o
<conj>      | string, unop, (, !
<conj'>     | -a
<literal>   | string, unop, (, !
<atom>      | string, unop, (
<atom'>     | binop

right side         | FIRST_1
-------------------+---------------------
<disj> EOF         | string, unop, (, !
<conj> <disj'>     | string, unop, (, !
-o <disj>          | -o
<literal> <conj'>  | string, unop, (, !
-a <conj>          | -a
<atom>             | string, unop, (
! <atom>           | !
string <atom'>     | string
unop string        | unop
( <disj> )         | (
binop string       | binop

nonterminal | FOLLOW_1
------------+--------------------
<S>         | \emptyset
<disj>      | EOF, )
<disj'>     | EOF, )
<conj>      | -o, EOF, )
<conj'>     | -o, EOF, )
<literal>   | -a, -o, EOF, )
<atom>      | -a, -o, EOF, )
<atom'>     | -a, -o, EOF, )

Hence we have the following requirements for being LL(1):

nonterminal | must be mutually disjoint
------------+--------------------------
<S>         | ---
<disj>      | ---
<disj'>     | EOF, ), -o 
<conj>      | ---
<conj'>     | -o, EOF, ), -a
<literal>   | string, unop, (, !
<atom>      | string, unop, (
<atom'>     | -a, -o, EOF, ), binop

*)
