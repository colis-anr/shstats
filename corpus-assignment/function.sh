#!/bin/dash

f () {
	x='set by f'
	y=42
}

x='set at toplevel'
echo $x
f
echo $x
