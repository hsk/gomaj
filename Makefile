all:
	cd src; omake

hello:
	./gomajjc example/Hello.gomaj example/Hello.java
	javac example/Hello.java
	java example.Hello

fib:
	./gomajc example/Fib.gomaj example/Fib.java
	javac example/Fib.java
	java example.Fib

calc: example/Calc.gomaj
	./gomajc example/Calc.gomaj example/Calc.java
	javac example/Calc.java
	java example.Calc

clean:
	cd src; omake clean
	rm -rf gomajc gomajc.opt example/*.java example/*.class src/.omakedb




