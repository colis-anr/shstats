#!/bin/dash

x=old
x=new /bin/test "whatever"
# we know statically that $x is "old" since the previous line is a process
echo $x
