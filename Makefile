all: foo.txt bar.txt
	echo all

foo.txt : baz.txt
	@ echo foo.txt

bar.txt :
	@ echo bar.txt
