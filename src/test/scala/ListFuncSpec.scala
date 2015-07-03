import org.scalatest._

class ListFuncSpec extends FlatSpec with Matchers {
  val lf = ListFunc

  "Last" should "find last element of list" in {
    lf.last(List(1, 1, 2, 3, 5, 8)).get == 8
  }

  "Last" should "return None when Nil or empty Lists" in {
    lf.last(Nil)    should be === None
    lf.last(List()) should be === None
  }

  "Penultimate" should "find penultimate element of list" in {
    lf.penultimate(List(1, 1, 2, 3, 5, 8)).get should be === 5
  }

  "Penultimate" should "return None when Nil or empty Lists" in {
    lf.penultimate(List()) should be === None
    lf.penultimate(Nil)    should be === None
  }

  "Nth" should "find nth element of list" in {
    lf.nth(2, List(1, 1, 2, 3, 5, 8)).get should be === 2
  }

  "Nth" should "return None when Nil or empty Lists, or when n is out of range" in {
    lf.nth(2, List())        should be === None
    lf.nth(2, Nil)           should be === None
    lf.nth(5, List(1,2,3,4)) should be === None
  }

  "Length" should "item count of list" in {
    lf.length(List(1, 1, 2, 3, 5, 8)) should be === 6
  }

  "IsPalindrome" should "return true when list is palindrome" in {
    lf.isPalindrome(List(1, 2, 3, 2, 1)) should be === true
    lf.isPalindrome(List(1,2,2,1))       should be === true
  }

  "IsPalindrome" should "return false when list is not palindrome" in {
    lf.isPalindrome(List(1, 2, 3, 1, 1)) should be === false
  }

}