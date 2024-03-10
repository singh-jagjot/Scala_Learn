trait Generator[+T]:
    def generate(): T
    def map[S](f: T => S) = new Generator[S]:
        def generate(): S = f(Generator.this.generate())

    def flatMap[S](f: T => Generator[S]) = new Generator[S]:
        def generate(): S = f(Generator.this.generate()).generate()

val integers = new Generator[Int]:
    val rand = java.util.Random()
    def generate(): Int = rand.nextInt()

def booleans = for x <- integers yield x > 0

def pairs[T, U](t: Generator[T], u: Generator[U]): Generator[(T, U)] =
    for x <- t; y <- u yield (x, y)

def single[T](x: T): Generator[T] = new Generator[T]:
    def generate(): T = x

def range(lo: Int, hi: Int): Generator[Int] = 
    for x <- integers yield lo + x.abs % (hi - lo)

def oneOf[T](xs: T*): Generator[T] = 
    for idx <- range(0, xs.length) yield xs(idx)

val choice = oneOf("red", "green", "blue")

choice.generate()

// def lists: Generator[List[Int]] = 
//     for
//         isEmpty <- booleans
//         list <- if isEmpty then emptyLists else nonEmptyLists
//     yield list

def lists: Generator[List[Int]] = 
    for 
        kind <- range(0, 5)
        list <- if kind == 0 then emptyLists else nonEmptyLists
    yield list

def emptyLists = single(List())

def nonEmptyLists = 
    for
        head <- integers
        tail <- lists
    yield head :: tail

lists.generate()


enum Tree:
    case Inner(left: Tree, right: Tree)
    case Leaf(x: Int)

def trees: Generator[Tree] = 
    for
        isLeaf <- booleans
        tree <- if isLeaf then leafs else inners
    yield tree

def leafs = for x <- integers yield Tree.Leaf(x)

def inners = for x <- trees; y <- trees yield Tree.Inner(x, y)

trees.generate()


def test[T](g: Generator[T], numTimes: Int = 100)(test: T => Boolean): Unit =
    for i <- 0 until numTimes do
        val value = g.generate()
        assert(test(value), s"test failed for $value")
    print(s"passed $numTimes tests")

test(pairs(lists, lists)) {
    (xs, ys) => (xs ++ ys).length >= xs.length
}

test(pairs(lists, lists), 50)((xs, ys) => (xs ++ ys).length >= xs.length)