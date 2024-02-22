package fpinscala.exercises.errorhandling

import scala.None as _
import scala.Option as _
import scala.Some as _

// Hide std library `Option` since we are writing our own in this chapter

enum Option[+A]:
  case Some(get: A)
  case None
  import Option.pure

  infix def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(a) => Some(f(a))

  def map_via_fmap[B](f: A => B): Option[B] =
    flatMap(f andThen pure)

  infix def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(a) => a

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case None    => None
    case Some(a) => f(a)

  private def flatMap_via_map[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if f(a) then Some(a) else None)

  private def filter_via_map(f: A => Boolean): Option[A] =
    map(a => if f(a) then Some(a) else None) getOrElse None

end Option

object Option:
  def pure[A](a: A): Option[A] = Some(a)

  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

  def failingFn(i: Int): Int =
    val y: Int = throw new Exception("fail!") /* `val y: Int = ...` declares `y`
     * as having type `Int`, and sets it equal to the right hand side of the
     * `=`. */
    try
      val x = 42 + 5
      x + y
    catch case e: Exception => 43 /* A `catch` block is just a pattern matching
       * block like the ones we've seen. `case e: Exception` is a pattern that
       * matches any `Exception`, and it binds this value to the identifier `e`.
       * The match returns the value 43. */

  def failingFn2(i: Int): Int =
    try
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) /* A thrown Exception can be
       * given any type; here we're annotating it with the type `Int` */
    catch case e: Exception => 43

  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  /* Implement the variance function in terms of flatMap .If the mean of a
   * sequence is m , the variance is the mean of math.pow(x - m, 2) for each
   * element x in the sequence */
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs map (x => math.pow(x - m, 2))))

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for
      aa <- a
      bb <- b
    yield f(aa, bb)

  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((oa, oas) => map2(oa, oas)(_ :: _))

  private def sequence_via_fmap[A](as: List[Option[A]]): Option[List[A]] =
    as match
      case Nil    => Some(Nil)
      case h :: t => h.flatMap(hh => sequence_via_fmap(t).map(hh :: _))

  private def traverse_via_mapseq[A, B](as: List[A])(
      f: A => Option[B]
  ): Option[List[B]] =
    sequence(as.map(a => f(a)))

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](pure(Nil))((a, oas) =>
      map2(f(a), oas)(_ :: _)
    )

  def sequence_via_traverse[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(a => a)
