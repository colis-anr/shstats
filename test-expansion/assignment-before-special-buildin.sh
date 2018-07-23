#!/bin/dash

x=old
x=new : /bin/echo
# we know statically that $x is "new" since : is a special buildin
echo $x
