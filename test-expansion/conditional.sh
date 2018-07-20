#!/bin/sh

x=1
if true; then
    y=2
    echo $x ${y}
else
    y=3
    echo  ${x} $y
fi
echo $x $y
