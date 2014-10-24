package example;
class Test {
  private int a;
  private int b = 1;
  private int c() {
    return 1;
  }
  public static void main(String[] argv) {
    System.out.println("c()="+ new Test().c());
    int a = 0;
    int b = 0;
    System.out.println("1+2+3="+(a=b=1+2+3));
    System.out.println("1+2+3="+(a=(b=1)+2+3));
    System.out.println("1*2+3="+(1*2+3));
    System.out.println("(1+2)*3="+(1+2)*3);
    System.out.println("-(1+2*3)="+ -(1+2*3));
    System.out.println("(-1+2*3)="+( -1+2*3));
    System.out.println("eq="+ new Test().eval( new Int(1)));
    System.out.println("eq="+ new Test().eval( new Add( new Int(1),  new Int(2))));
  }
  private int eval(E e) {
    
    if (e instanceof Int) {
      Int $ = (Int)e;
      return $._1;
    }

    if (e instanceof Add) {
      Add $ = (Add)e;
      return eval($._1)+eval($._2);
    }

    return 0;
  }
}
class Point {
  Point(int x, int y) {
    this.x=x;
    this.y=y;
  }
  int x;
  int y;
}
class Point3D {
  Point3D(int x, int y, int z) {
    this.x=x;
    this.y=y;
    this.z=z;
  }
  int x;
  int y;
  int z;
}
class E {
  E() {
  }
}
class Int extends E {
  Int(int _1) {
    this._1=_1;
  }
  int _1;
}
class Add extends E {
  Add(E _1, E _2) {
    this._1=_1;
    this._2=_2;
  }
  E _1;
  E _2;
}