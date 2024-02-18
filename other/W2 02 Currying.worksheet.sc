def product(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1 else f(a) * product(f)(a + 1, b)

product(x => x * x)(1, 5)

def fact(n: Int): Int = product(x => x)(1, n)

fact(5)

def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(
    a: Int,
    b: Int
): Int =
  def recur(a: Int): Int =
    if a > b then zero
    else combine(f(a), recur(a + 1))
  recur(a)

def sum(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)

sum(fact)(1, 5)

def newProduct(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)

newProduct(x => x * x)(1, 5)
