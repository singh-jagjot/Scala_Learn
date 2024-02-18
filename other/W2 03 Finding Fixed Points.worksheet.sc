val tolerance = 0.0001

def abs(x: Double) = if x >= 0 then x else -x

def isCloseEnough(x: Double, y: Double): Boolean =
  abs((x - y) / x) < tolerance

def averageDamp(f: Double => Double)(x: Double): Double =
  (x + f(x)) / 2

def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
  def iterate(guess: Double): Double =
    val next = f(guess)
    if isCloseEnough(guess, next) then next
    else iterate(next)
  iterate(firstGuess)

def sqrt(x: Double): Double =
  fixedPoint(averageDamp(y => x / y))(1.0)

print(sqrt(2))
