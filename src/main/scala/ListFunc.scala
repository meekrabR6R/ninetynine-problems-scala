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
}
