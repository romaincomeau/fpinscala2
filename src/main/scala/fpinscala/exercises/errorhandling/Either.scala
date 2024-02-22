package fpinscala.exercises.errorhandling

import scala.util.control.NonFatal
// Hide std library `Either` since we are writing our own in this chapter
import scala.Either as _
import scala.Left as _
import scala.Right as _

enum Either[+E, +A]:
  case Left(get: E)
  case Right(get: A)

  def map[B](f: A => B): Either[E, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => Left(e)
    case Right(a) => f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match
    case Left(e) => b
    case _       => this

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for
    e1 <- this
    e2 <- b
  yield f(e1, e2)

object Either:
  def pure[E, A](a: A): Either[E, A] = Right(a)

  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](pure(Nil))((a, b) => f(a).map2(b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of empty list!")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] =
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](
      a: Either[List[E], A],
      b: Either[List[E], B],
      f: (A, B) => C
  ): Either[List[E], C] = (a, b) match
    case (Right(aa), Right(bb)) => Right(f(aa, bb))
    case (Left(ee), Right(_))   => Left(ee)
    case (Right(_), Left(ee))   => Left(ee)
    case (Left(es1), Left(es2)) => Left(es1 ++ es2)

  def traverseAll[E, A, B](
      as: List[A],
      f: A => Either[List[E], B]
  ): Either[List[E], List[B]] =
    as.foldRight[Either[List[E], List[B]]](pure(Nil))((a, b) =>
      map2All(f(a), b, _ :: _)
    )

  def sequenceAll[E, A](
      as: List[Either[List[E], A]]
  ): Either[List[E], List[A]] =
    traverseAll(as, a => a)
