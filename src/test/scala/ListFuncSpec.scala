import org.scalatest._

class ListFuncSpec extends FlatSpec with Matchers {
  val lf = ListFunc

  "Last" should "find last element of list" in {
    lf.last(List(1, 1, 2, 3, 5, 8)).get == 8
  }

  "Last" should "return None when Nil or empty Lists" in {
    lf.last(Nil) == None
    lf.last(List()) == None
  }

  "Penultimate" should "find penultimate element of list" in {
    lf.penultimate(List(1, 1, 2, 3, 5, 8)).get == 5
  }

  "Penultimate" should "return None when Nil or empty Lists" in {
    lf.penultimate(List()) == None
    lf.penultimate(Nil) == None
  }
}