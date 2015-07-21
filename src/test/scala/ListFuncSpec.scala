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

  "Flatten" should "return a flat list of type List[Any] from a list of  List[Any]'s and Any's" in {
    lf.makeFlat(List(List(1, 1), 2, List(3, List(5, 8)))) should be === List(1, 1, 2, 3, 5, 8)
  }

  "Flatten" should "return an empty List for Nil or an empty List" in {
    lf.makeFlat(Nil) should be === List()
    lf.makeFlat(List()) should be === List()
  }

  "Compress" should "replace repeated consecutive elements with a single copy of that element" in {
    lf.compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be === List('a, 'b, 'c, 'a, 'd, 'e)
  }

  "Compress" should "should return an empty List for Nil or an empty List" in {
    lf.compress(Nil) should be === List()
    lf.compress(List()) should be === List()
  }

  "Pack" should "place repeated elements into separate sublists" in {
    lf.pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be ===
      List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  }

  "Encode" should "encode consecutive duplicates of elements as tuples (N, E) where N is the number of duplicates of the element E." in {
    lf.encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be === List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  }

  "Encode" should "should return an empty List for Nil or an empty List" in {
    lf.encode(Nil) should be === List()
    lf.encode(List()) should be === List()
  }

}