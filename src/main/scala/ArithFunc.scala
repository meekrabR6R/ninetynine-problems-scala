/**
 * Solutions to Arithmetic Problems
 * Created by nickmiano on 9/8/15.
 */
object ArithFunc {
  /**
   * P31 (**) Determine whether a given integer number is prime.
   * @param n
   * @return
   */
  def isPrime(n: Int): Boolean = {
    def helper(curr: Int): Boolean =
      if (curr == 1) true
      else if (curr > 1 && (n % curr == 0)) false
      else helper(curr - 1)

    helper(n - 1)
  }

  /**
   * P32 (**) Determine the greatest common divisor of two positive integer numbers.
   * Use Euclid's algorithm.
   * @param n
   * @param m
   * @return
   */
  def gcd(n: Int, m: Int): Int =
    if (n == 0) m
    else if (m == 0) n
    else gcd(m, n % m)
}
