package fpinscala.exercises
package monads

import parallelism.*
import parallelism.Par.*
import parsing.*
import state.*
import testing.*

trait Functor[F[_]]:
  extension [A](fa: F[A]) def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)])
    def distribute: (F[A], F[B]) =
      (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]])
    def codistribute: F[Either[A, B]] =
      e match
        case Left(fa)  => fa.map(Left(_))
        case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List] with
    extension [A](as: List[A]) def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

    def product[B](fb: F[B]): F[(A, B)] =
      fa.map2(fb)((_, _))

    def **[B](fb: F[B]): F[(A, B)] =
      fa.product(fb)

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List.empty[B]))((h, t) => f(h).map2(t)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    traverse(fas)(identity)

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    sequence(List.fill(n)(fa))

  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldRight(unit(List[A]()))((h, t) =>
      f(h).flatMap(b => if b then unit(h).map2(t)(_ :: _) else t)
    )

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      compose(_ => fa, f)(())

  extension [A](ffa: F[F[A]])
    def join: F[A] =
      ffa.flatMap(identity)

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

  def compose[A, B, C](fa: A => F[B], fb: B => F[C]): A => F[C] =
    a => fa(a).flatMap(fb)

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).map(g).join

end Monad

object Monad:
  given stateMonad[S]: Monad[State[S, _]] with
    def unit[A](a: => A) = State.unit(a)
    extension [A](fa: State[S, A])
      override def flatMap[B](f: A => State[S, B]): State[S, B] =
        State.flatMap(fa)(f)

  given genMonad: Monad[Gen] with
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par] with
    def unit[A](a: => A) = Par.unit(a)
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        Par.flatMap(fa)(f)

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = p.succeed(a)
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        p.flatMap(fa)(f)

  given optionMonad: Monad[Option] with
    def unit[A](a: => A) = Some(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa.map(f).getOrElse(None)

  given lazyListMonad: Monad[LazyList] with
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        fa.flatMap(f)

  given listMonad: Monad[List] with
    def unit[A](a: => A) = a :: Nil
    // extension [A](fa: List[A])
    //   override def flatMap[B](f: A => List[B]) =
    //     fa.flatMap(f)

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] =
    f(value)

object Id:
  given idMonad: Monad[Id] with
    def unit[A](a: => A) = Id(a)
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        fa.flatMap(f)



// READER
// 1. What are its primitive operations? FlatMap, Pure
// 2. Sequence would allow to describe a sequence of operations that would be chained
// allowing us to go from a List[Reader[R, A]] to a Reader[R, List[A]].
// 3. replicateM would repeat a Reader `n` times passing in the reader automatically.
opaque type Reader[-R, +A] = R => A
object Reader:
  def ask[R]: Reader[R, R] = r => r

  extension [R, A](ra: Reader[R, A]) def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    // We ignore the provided reader and simply return _a_
    def unit[A](a: => A): Reader[R, A] = _ => a

    extension [A](fa: Reader[R, A])
      // We describe a computation that will take a (reader: R),
      // computes its result, then using the `f` function
      // allows to choose the following Reader that will have access
      // to the (reader: R parameter).
      override def flatMap[B](f: A => Reader[R, B]) =
        r => f(fa(r))(r)

end Reader
