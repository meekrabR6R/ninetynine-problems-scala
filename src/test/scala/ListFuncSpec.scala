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

  "Compress" should "return an empty List for Nil or an empty List" in {
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

  "Encode" should "return an empty List for Nil or an empty List" in {
    lf.encode(Nil) should be === List()
    lf.encode(List()) should be === List()
  }

  "EncodeMod" should "modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied " +
                     "into the result list. Only elements with duplicates are transferred as (N, E) terms." in {
    lf.encodeMod(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be === List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  }

  "EncodeMod" should "return an empty List for Nil or an empty List" in {
    lf.encode(Nil) should be === List()
    lf.encode(List()) should be === List()
  }

  "Decode" should ", given a run-length code list generated as specified in problem P10, construct its uncompressed version." in {
    lf.decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be === List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  }

  "Decode" should "return an empty List for Nil or an empty List" in {
    lf.decode(Nil) should be === List()
    lf.decode(List()) should be === List()
  }

  "EncodeDirect" should "encode consecutive duplicates of elements as tuples (N, E) where N is the number of duplicates of the element E." in {
    lf.encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }

  "EncodeDirect" should "return an empty List for Nil or an empty List" in {
    lf.encodeDirect(Nil) should be === List()
    lf.encodeDirect(List()) should be === List()
  }

  "Duplicate" should "duplicate elements in a list" in {
    lf.duplicate(List('a, 'b, 'c, 'c, 'd)) should be === List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  }

  "Duplicate" should "return an empty List for Nil or an empty List" in {
    lf.duplicate(Nil) should be === List()
    lf.duplicate(List()) should be === List()
  }

  "DuplicateN" should "duplicate elements in a list n times" in {
    lf.duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be === List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  }

  "DuplicateN" should "return an empty List for Nil or an empty List" in {
    lf.duplicateN(3, Nil) should be === List()
    lf.duplicateN(3, List()) should be === List()
  }

  "Drop" should "drop every Nth element from list" in {
    lf.drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be === List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  }

  "Drop" should "return an empty List for Nil or an empty List" in {
    lf.drop(3, Nil) should be === List()
    lf.drop(3, List()) should be === List()
  }

  "Split" should "split a list into two parts and return a Tuple containing both parts" in {
    lf.split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be === (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  }

  "Split" should "return an empty List for Nil or an empty List" in {
    lf.split(3, Nil) should be === (List(), List())
    lf.split(3, List()) should be === (List(), List())
  }

  "Slice" should "return an empty List for Nil or an empty List" in {
    lf.slice(3, 7, Nil) should be === List()
    lf.slice(3, 7, List()) should be === List()
  }

  "Slice" should "return an empty list if i > k" in {
    lf.slice(5, 3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be === List()
  }

  "Rotate" should "split a list into two parts and return a Tuple containing both parts" in {
    lf.rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be === List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
    lf.rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be === List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  }

  "Rotate" should "return an empty List for Nil or an empty List" in {
    lf.rotate(3, Nil) should be === List()
    lf.rotate(3, List()) should be === List()
  }

  "RemoveAt" should "remove the Kth element from a List" in {
    lf.removeAt(1, List('a, 'b, 'c, 'd)) should be === (List('a, 'c, 'd), 'b)
  }

  "RemoveAt" should "throw and IndexOutOfBoundsException" in {
    intercept[IndexOutOfBoundsException] { lf.removeAt(3, Nil) }
    intercept[IndexOutOfBoundsException] { lf.removeAt(3, List()) }
    intercept[IndexOutOfBoundsException] { lf.removeAt(3, List(0,1,2)) }
  }

  "InsertAt" should "insert an element at a given position into a list." in {
    lf.insertAt('new, 1, List('a, 'b, 'c, 'd)) should be === List('a, 'new, 'b, 'c, 'd)
  }
}