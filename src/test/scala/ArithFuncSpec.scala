import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by nickmiano on 9/8/15.
 */
class ArithFuncSpec extends FlatSpec with Matchers {
  val af = ArithFunc

  "IsPrime" should "determine if a number is prime or not" in {
    af.isPrime(7) should be === true
    af.isPrime(4) should be === false
  }

  "GCD" should "determine the greatest common divisor of two positive integers" in {
    af.gcd(36, 63) should be === 9
  }
}
