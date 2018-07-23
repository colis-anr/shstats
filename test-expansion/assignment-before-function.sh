#!/bin/dash

f() {
    echo coocoo
}
x=old
x=new f
# we don't know statically the value of $x
echo $x
