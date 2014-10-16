all:
	cd src; omake

hello:
	./gomajc example/Hello.gomaj example/Hello.java
	javac example/Hello.java
	java example.Hello

fib:
	./gomac example/fib.goma example/fib.cpp
	g++ example/fib.cpp -o fib
	./fib

calc: example/calc.goma
	./gomac example/calc.goma example/calc.cpp
	g++ example/calc.cpp -o calc
	./calc

clean:
	cd src; omake clean
	rm -rf gomac gomac.opt hello calc fib example/*.cpp src/.omakedb
