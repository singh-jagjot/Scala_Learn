val nums = List(1, 2, 3, 4, 5, 6)
nums.partition(p => p % 2 != 0)
nums.span(p => p % 2 != 0)

def pack[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case x :: xs1 =>
    val (more, rest) = xs1.span(y => y == x)
    (x :: more) :: pack(rest)

val elems = List("a", "a", "a", "b", "c", "c", "a")
pack(elems)


def encode[T](xs: List[T]): List[(T, Int)] = pack(xs).map(x => (x.head, x.length))

encode(elems)


def concat[T](xs: List[T], ys: List[T]): List[T] =
  xs.foldRight(ys)(_ :: _)
  // xs.foldLeft(ys)(_ :: _)

val a =List(1,2,3,4)
val b = List(5,6,7,8 )

concat(a, b)