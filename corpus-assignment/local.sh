#!/bin/bash

f () {
	local x
	x=73
	g () {
		x=42
	}
}

x=17
echo $x
f
echo $x
g
echo $x


