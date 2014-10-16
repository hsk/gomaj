
function fib(n) {
  if (n < 2) return 1;
  return fib(n - 1) + fib(n - 2);
}

function Fib(x) {
	this.x = x;
}

Fib.prototype.fib = function() {
    if (this.x < 2) return 1;
    return new Fib(this.x-2).fib() + new Fib(this.x-1).fib();
};

function tim() {
	return new Date().getTime();
}

function fib2(fib) {
	if(fib.x < 2) return 1;
	return fib2(new Fib(fib.x-2))+fib2(new Fib(fib.x-1));
}

function fib3(fib) {
	if(fib.x < 2) return 1;
	return fib3({x:fib.x-2})+fib3({x:fib.x-1});
}

function main() {
  var start = tim();
  console.log(fib(40));
  console.log(tim()-start+"ms");

  start = tim();
  console.log(new Fib(40).fib());
  console.log(tim()-start+"ms");

  start = tim();
  console.log(fib2(new Fib(40)));
  console.log(tim()-start+"ms");

  start = tim();
  console.log(fib3({x:40}));
  console.log(tim()-start+"ms");
}

main();
