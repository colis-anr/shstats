#!/bin/sh

x=1
# the effects of the assignment to x and to z are only effective starting
# on the following line.
x=2 y=${z:=3} /bin/echo $x $z
echo $x
echo $z
