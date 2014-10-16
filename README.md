# GomaJ Programming Language

GomaJ is next generation JVM Programming Language.

GomaJ translate to Java.

## install

    $ make

## hello world

example/Hello.gomaj

```
package example
Hello class {
  public static main():void= {
    System.out.println("hello world")
  }
}
```

    $ ./javac example/hello.gomaj example/Hello.java
    $ javac example/Hello.java
    $ ./java example.Hello
    hello world!

example/Hello.java

```
package example;
class Hello {
  public static void main() {
    System.out.println("hello world!");
  }
}
```

