import scala.util.Random

/**
 * Solutions to List problems
 * TODO: Problems 27 and 28
 */
object ListFunc {

  //test
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

  /**
   * P16 (**) Drop every Nth element from a list.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def drop[A](n: Int, xs: List[A]): List[A] = {
    def helper(idx: Int, ys: List[A], acc: List[A]): List[A] = ys match {
      case y::ys => if (idx == (n - 1)) helper(0, ys, acc) else helper(idx+1, ys, y::acc)
      case _     => acc.reverse
    }
    helper(0, xs, List())
  }

  /**
   * P17 (*) Split a list into two parts.
   * The length of the first part is given. Use a Tuple for your result.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def split[A](n: Int, xs: List[A]):(List[A], List[A]) = (xs.take(n), xs.takeRight(xs.size - n)) //xs.splitAt(n) //lol is this cheating?

  /**
   * P18 (**) Extract a slice from a list.
   * Given two indices, I and K, the slice is the list containing the
   * elements from and including the Ith element up to but not including
   * the Kth element of the original list. Start counting the elements with 0.
   * @param i
   * @param k
   * @param xs
   * @tparam A
   * @return
   */
  def slice[A](i: Int, k: Int, xs: List[A]): List[A] = xs.drop(i).take(k - i)

  /**
   * P19 (**) Rotate a list N places to the left.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def rotate[A](n: Int, xs: List[A]): List[A] =
    if (n > 0) xs.drop(n) ++ xs.take(n) else xs.takeRight(-n) ++ xs.dropRight(-n)

  /**
   * P20 (*) Remove the Kth element from a list.
   * Return the list and the removed element in a Tuple.
   * Elements are numbered from 0.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def removeAt[A](n: Int, xs: List[A]): (List[A], A) = (xs.take(n) ++ xs.takeRight(n + 1), xs(n))

  /**
   * P21 (*) Insert an element at a given position into a list.
   * @param el
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def insertAt[A](el: A, n: Int, xs: List[A]):List[A] = xs.take(n) ++ (el::xs.takeRight(xs.size - n))

  /**
   * P22 (*) Create a list containing all integers within a given range.
   * @param x
   * @param y
   * @return
   */
  def range(x: Int, y: Int): List[Int] = {
    def helper(currCount: Int, currAcc: List[Int]): List[Int] = currCount match {
      case n if n == y => (n::currAcc).reverse
      case _           => if (y < x) helper(currCount - 1, currCount::currAcc)
                          else helper(currCount + 1, currCount::currAcc)
    }
    helper(x, List())
  }

  /**
   * P23 (**) Extract a given number of randomly selected elements from a list.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def randomSelect[A](n: Int, xs: List[A]):List[A] = Random.shuffle(xs).take(n)

  /**
   * P24 (*) Lotto: Draw N different random numbers from the set 1..M.
   * @param n
   * @param m
   * @return
   */
  def lotto(n: Int, m: Int): List[Int] = Random.shuffle((0 until m).toList).take(n)

  /**
   * P25 (*) Generate a random permutation of the elements of a list.
   * @param xs
   * @tparam A
   * @return
   */
  def randomPermute[A](xs: List[A]): List[A] = Random.shuffle(xs)

  /**
   * P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
   * For example, how many ways can a committee of 3 be chosen from a group of 12 people? We all know that
   * there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient).
   * For pure mathematicians, this result may be great.
   * But we want to really generate all the possibilities.
   * @param n
   * @param xs
   * @tparam A
   * @return
   */
  def combinations[A](n: Int, xs: List[A]): List[List[A]] = xs.combinations(n).toList //TODO: THIS IS CHEATING!! COME BACK WITH A REAL ANSWER!
}

