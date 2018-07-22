#!/bin/sh

if (x=1 y=2 true); then
    x=3
    echo $x $y $z
else
    z=4
    echo $x $y $z
fi
echo $x $y $z
