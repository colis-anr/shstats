#!/bin/sh

f () {
    echo $1 $2
}
x=1
# the effects of the assignment to x and to z are only effective starting
# on the following line.
x=2 y=${z:=3} f $x $z # statically: $x is 1, $z is unknown
# after invokation of a function we statically loose all bindings. 
echo $x
echo $z
