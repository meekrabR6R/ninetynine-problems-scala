import org.scalatest._

class ListFuncSpec extends FlatSpec with Matchers {
  val lf = ListFunc

  "Last" should "find last element of list" in {
    lf.last(List(1, 1, 2, 3, 5, 8)).get == 8
  }

  "Nil or empty Lists" should "return None" in {
    lf.last(Nil) == None
    lf.last(List()) == None
  }
}