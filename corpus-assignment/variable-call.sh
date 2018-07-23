f () {
	y=7
	echo "this is f"
}

g() {
	echo "this is g"
}

x=f
$x
x=g
$x


