extern crate std;

struct EInt {x:int}
struct EAdd {x:Box<E>,y:Box<E>}
struct EMul {x:Box<E>,y:Box<E>}

trait E         { fn eval(&self) -> int; }
impl E for EInt { fn eval(&self) -> int { self.x } }
impl E for EAdd { fn eval(&self) -> int { self.x.eval() + self.y.eval() } }
impl E for EMul { fn eval(&self) -> int { self.x.eval() * self.y.eval() } }

fn main() {

  let i = EInt{x:1};
  println!("eval 1 = {}", i.eval());

  let add = EAdd{x:box EInt{x:1},y:box EInt{x:2}};
  println!("eval 1 + 2 = {}", add.eval());

  let mul = EMul{x:box EAdd{x:box EInt{x:1},y:box EInt{x:2}},y:box EInt{x:111}};
  println!("eval (1 + 2) * 111 = {}", mul.eval());
}
