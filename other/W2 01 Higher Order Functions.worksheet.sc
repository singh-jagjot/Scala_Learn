import scala.annotation.tailrec

def old_sum(f: Int => Int, a: Int, b: Int): Int =
  if a > b then 0 else f(a) + old_sum(f, a + 1, b)

def sum(f: Int => Int, a: Int, b: Int): Int =
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if a > b then acc
    else loop(a + 1, acc + f(a))
  loop(a, 0)


println(old_sum(x => x, 1, 5))
println(sum(x => x, 1, 5))
