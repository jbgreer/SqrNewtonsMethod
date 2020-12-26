object Session {

  def abs(x: Double) = if (x < 0) -x else x


def sqrt(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double): Boolean =
      abs(guess * guess - x) / x < 0.001

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }

  //sqrt(0.001)
  //sqrt(0.1e-20)
  //sqrt(1.0e20)
  //sqrt(1.0e50)

  def factorial(n: Integer): Integer = {
    def f(acc: Integer, n: Integer): Integer =
      if (n == 0) acc else f(n * acc, n-1)

    f(1, n)
  }

  factorial(4)

}