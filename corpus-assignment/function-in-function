#!/bin/dash

f () {
	x='set by f'
	g () {
		x='set by g'
	}
}

x='set at toplevel'
echo $x
f
echo $x
g
echo $x
