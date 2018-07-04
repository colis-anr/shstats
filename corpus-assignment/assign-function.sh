#!/bin/dash

f () {
	/bin/echo $*
   }	  

x=old
x=new f "this is a function"
echo $x
