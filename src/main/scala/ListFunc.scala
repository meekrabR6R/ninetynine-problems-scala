/**
 * Solutions to List problems
 */
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

  /**
   * P08 (**) Eliminate consecutive duplicates of list elements.
   * If a list contains repeated elements they should be replaced with
   * a single copy of the element. The order of the elements
   * should not be changed.
   * @param xs
   * @tparam A
   * @return
   */
  def compress[A](xs: List[A]): List[A] = xs match {
    case x::xs => xs.foldLeft(List(x))((acc, y) => if (acc.head == y) acc else y::acc).reverse
    case _     => xs
  }

  /**
   * P09 (**) Pack consecutive duplicates of list elements into sublists.
   * If a list contains repeated elements they should be placed in separate sublists.
   * @param xs
   * @tparam A
   * @return
   */
  def pack[A](xs: List[A]): List[List[A]] = xs match {
    case x::xs => xs.foldLeft(List(List(x))){ (acc, y) =>
      if (acc.head.contains(y)) (y::acc.head)::acc.tail
      else List(y)::acc
    }.reverse


    case _     => Nil
  }

  /**
   * P10 (*) Run-length encoding of a list.
   * Use the result of problem P09 to implement
   * the so-called run-length encoding data compression method.
   * Consecutive duplicates of elements are encoded as tuples (N, E)
   * where N is the number of duplicates of the element E.
   * @param xs
   * @tparam A
   * @return
   */
  def encode[A](xs: List[A]): List[(Int, A)] = pack(xs) map { x => (x.size, x.head) }

  /**
   * P11 (*) Modified run-length encoding.
   * Modify the result of problem P10 in such a way that if an element
   * has no duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   * (modifying a copy of P10 to document both answers)
   * @param xs
   * @tparam A
   * @return
   */
  def encodeMod[A](xs: List[A]): List[Any] = pack(xs) map { x => if (x.size > 1) (x.size, x.head) else x.head }

  /**
   * P12 (**) Decode a run-length encoded list.
   * Given a run-length code list generated as specified in problem P10,
   * construct its uncompressed version.
   * @param xs
   * @tparam A
   * @return
   */
  def decode[A](xs: List[(Int, A)]): List[A] = xs.foldRight(List[A]()) { (x, acc) => List.fill(x._1)(x._2)++acc }

  /**
   * P13 (**) Run-length encoding of a list (direct solution).
   * Implement the so-called run-length encoding data compression method directly.
   * I.e. don't use other methods you've written (like P09's pack); do all the work directly.
   * @param xs
   * @tparam A
   * @return
   */
  def encodeDirect[A](xs: List[A]): List[(Int, A)] =
    xs.foldRight(List[(Int, A)]()) { (x, acc) =>
      if ((acc.length > 0) && (x == acc.head._2)) (acc.head._1 + 1, x)::acc.tail
      else (1, x)::acc
    }.reverse

  /**
   * P14 (*) Duplicate the elements of a list.
   * @param xs
   * @tparam A
   * @return
   */
  def duplicate[A](xs: List[A]): List[A] = xs.flatMap { x => List.fill(2)(x) }

  /**
   * P15 (**) Duplicate the elements of a list a given number of times.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def duplicateN[A](n: Int, xs: List[A]): List[A] = xs.flatMap { x => List.fill(n)(x) }
}
