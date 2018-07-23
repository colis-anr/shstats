#!/bin/sh

x=1
# the effects of the assignment to x and to z are not visible to the shell
# on the same line, Since we have a process creation only the assignment
# to z is effective on the following line.
x=2 y=${z:=3} /bin/echo $x $z
echo $x
echo $z
