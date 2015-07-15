object ListFunc {

  type ??? = Nothing

  /**
   * P01 (*) Find the last element of a list.
   * @param xs
   * @tparam A
   * @return
   */
  def last[A](xs: List[A]): Option[A] = xs match {
    case Nil    => None
    case x::Nil => Some(x)
    case x::xs  => last(xs)
  }

  /**
   * P02 (*) Find the last but one element of a list.
   * @param xs
   * @tparam A
   * @return
   */
  def penultimate[A](xs: List[A]): Option[A] = xs match {
    case Nil       => None
    case x::y::Nil => Some(x)
    case x::xs     => penultimate(xs)
  }

  /**
   * P03 (*) Find the Kth element of a list.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def nth[A](n: Int, xs: List[A]): Option[A] = {
    def helper(k: Int, ys: List[A]): Option[A] =
      if (k == n) Some(ys.head)
      else helper(k + 1, ys.tail)

    if (n > xs.size) None
    else helper(0, xs)
  }

  /**
   * P04 (*) Find the number of elements of a list.
   * @param xs
   * @return
   */
  def length(xs: List[Any]): Int = {
    def helper(acc: Int, ys: List[Any]): Int = ys match {
      case Nil   => acc
      case y::ys => helper(acc + 1, ys)
    }
    helper(0, xs)
  }

  /**
   * P06 (*) Find out whether a list is a palindrome.
   * @param xs
   * @return
   */
  def isPalindrome(xs: List[Any]) = {
    val tup = xs.splitAt(xs.size / 2)
    val x1  = tup._1
    val x2  = tup._2

    if (x1.size == x2.size) x1 == x2.reverse
    else x1 == x2.tail.reverse
  }

  /**
   * P07 (**) Flatten a nested list structure.
   * @param xs
   * @return
   */
  def makeFlat(xs: List[Any]): List[Any] = xs flatMap {
    case x: List[Any] => makeFlat(x)
    case x: Any  => List(x)
  }
}
