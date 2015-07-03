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
    case x::y::Nil => Some(y)
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

  def isPalindrome(xs: List[Any]) = false
}
