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
      else helper(k+1, ys.tail)

    if (n > xs.size) None
    else helper(0, xs)
  }
}
