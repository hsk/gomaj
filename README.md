# GomaJ Programming Language

GomaJ is simple translator to Java language, namely, GomaJ is AltJ.

This compiler made by OCaml, OCamlYacc and OCamlLex.

## build

    $ make

## hello world

#### example/Hello.gomaj

```
package example
Hello class {
  public static main():void = {
    System.out.println("hello world")
  }
}
```

#### build & run

    $ ./javac example/hello.gomaj example/Hello.java
    $ javac example/Hello.java
    $ ./java example.Hello
    hello world!

or

    $ make hello

#### example/Hello.java

```
package example;
class Hello {
  public static void main() {
    System.out.println("hello world!");
  }
}
```

## examples

    $ make fib
    $ make test

## clean

    $ make clean

## License

MIT Licence.

