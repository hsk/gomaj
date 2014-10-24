package example;
class Fib {
  static int fib(int a) {
    if (a==0)
      return 0;
    else if (a==1)
      return 1;
    else
      return fib(a-2)+fib(a-1);
  }
  public static void main(String[] argv) {
    System.out.println("fib 10 = "+fib(10));
    System.out.println("Int.fib 10 = "+ new Int(10).fib());
  }
}
class Int {
  Int(int x) {
    this.x=x;
  }
  int x;
  int fib() {
    if (this.x==0) {
      return 0;
    } else if (this.x==1)
      return 1;
    else
      return  new Int(this.x-1).fib()+ new Int(this.x-2).fib();
  }
}