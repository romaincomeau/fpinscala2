package fpinscala.exercises.applicative

import fpinscala.answers.monads.Functor
import fpinscala.answers.monoids.Monoid
import fpinscala.answers.state.State

trait Monad[F[_]] extends Applicative[F]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

  override def apply[A, B](ff: F[A => B])(fa: F[A]): F[B] =
    ff.flatMap(f => fa.map(f))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(g)

  extension [A](ffa: F[F[A]]) def join: F[A] = ffa.flatMap(identity)

object Monad:
  def composeM[G[_], H[_]](
      using G: Monad[G],
      H: Monad[H],
      T: Traverse[H]
  ): Monad[[x] =>> G[H[x]]] = new:
    def unit[A](a: => A): G[H[A]] = ???
    extension [A](gha: G[H[A]])
      override def flatMap[B](f: A => G[H[B]]): G[H[B]] =
        ???

  given eitherMonad[E]: Monad[Either[E, _]] with
    def unit[A](a: => A): Either[E, A] = Right(a)
    extension [E, A](ma: Either[E, A])
      def flatMap[B](f: A => Either[E, B]): Either[E, B] = ma match
        case Right(a) => f(a)
        case Left(e)  => Left(e)
