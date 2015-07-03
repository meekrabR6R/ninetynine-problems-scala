object ListFunc {

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
}
