def from(n: Int): LazyList[Int] = n #:: from(n + 1)

val nats = from(0)

nats.take(10)

nats.take(10).toList

// infinite call
// nats.toList

def sieve(s: LazyList[Int]): LazyList[Int] =
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))

val primes = sieve(from(2))

primes.take(100).toList

// @main
// def lol(): Unit =
//     println(primes.take(10000).toList)

def sqrtSeq(x: Double): LazyList[Double] = 
    def improve(guess: Double) = (guess + x / guess) / 2
    // here lazy is required else compiler gives error due to builtin 
    // checks that Scala have. lazy will avoid the check.
    lazy val guesses: LazyList[Double] = 1 #:: guesses.map(improve)
    guesses

def isGoodEnough(guess: Double, x: Double) = ((guess * guess - x) / x).abs < 0.0001

sqrtSeq(2).filter(isGoodEnough(_, 2)).head