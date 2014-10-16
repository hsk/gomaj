function inherits(child, parent) {
  child.prototype = Object.create(parent.prototype, {
    constructor: {
      value: child,
      enumerable: false,
      writable: true,
      configurable: true
    }
  });
}


function E() {}
function EInt(x) { this.x = x; } inherits(EInt, E);
function EAdd(x, y) { this.x = x; this.y = y; } inherits(EAdd, E);
function EMul(x, y) { this.x = x; this.y = y; } inherits(EMul, E);

EInt.prototype.eval = function() { return this.x; };
EAdd.prototype.eval = function() { return this.x.eval() + this.y.eval(); };
EMul.prototype.eval = function() { return this.x.eval() * this.y.eval(); };

function main() {

  var i = new EInt(1);
  console.log("eval 1 = " + i.eval());

  var add = new EAdd(new EInt(1), new EInt(2));
  console.log("eval 1 + 2 = " + add.eval());

  var mul = new EMul(new EAdd(new EInt(1), new EInt(2)), new EInt(111));
  console.log("eval (1 + 2) * 111 = " + mul.eval());

}

main();
