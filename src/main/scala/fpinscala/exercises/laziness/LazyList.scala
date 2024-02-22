package fpinscala.exercises.laziness

import scala.annotation.tailrec

import LazyList.cons
import LazyList.empty
import LazyList.unfold

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def loop(rest: LazyList[A]): List[A] = rest match
      case Empty      => buf.toList
      case Cons(h, t) => buf += h(); loop(t())
    loop(this)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = /* The arrow `=>` in front
     * of the argument type `B` means that the function `f` takes its second
     * argument by name and may choose not to evaluate it. */
    this match
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) /* If `f` doesn't evaluate
         * its second argument, the recursion never occurs. */
      case _          => z

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) /* Here `b` is the unevaluated
     * recursive step that folds the tail of the lazy list. If `p(a)` returns
     * `true`, `b` will never be evaluated and the computation terminates early. */

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def find_1(p: A => Boolean): Option[A] =
    filter(p).headOption

  def take(n: Int): LazyList[A] = this match
    case Cons(h, t) if n > 1  => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _                    => empty

  @tailrec
  final def drop(n: Int): LazyList[A] = this match
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _                   => this

  def takeWhile(p: A => Boolean): LazyList[A] = this match
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _                    => empty

  def takeWhile_via_foldRight(p: A => Boolean): LazyList[A] =
    foldRight(empty[A])((h, t) => if p(h) then cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] = this match
    case Empty      => None
    case Cons(h, t) => Some(h())

  def headOption_via_foldRight: Option[A] =
    foldRight(None: Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): LazyList[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def append[A2 >: A](ll: => LazyList[A2]): LazyList[A2] =
    foldRight(this)((h, t) => cons(h, t))

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def filter(f: A => Boolean): LazyList[A] =
    foldRight(empty[A])((a, acc) => if f(a) then cons(a, acc) else acc)

  def map_via_unfold[B](f: A => B): LazyList[B] =
    unfold(this):
      case Empty      => None
      case Cons(h, t) => Some(f(h()) -> t())

  def take_via_unfold(n: Int): LazyList[A] =
    unfold(this -> n):
      case (Cons(h, t), 1)          => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _                        => None

  def takeWhile_via_unfold(p: A => Boolean): LazyList[A] =
    unfold(this):
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _                    => None

  def zipWith[B, C](that: LazyList[B])(f: (A, B) => C): LazyList[C] =
    unfold(this -> that) {
      case (Empty, _)                   => None
      case (_, Empty)                   => None
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), t1() -> t2()))
    }

  def zipAll[B](that: LazyList[B]): LazyList[(Option[A], Option[B])] =
    unfold(this -> that) { (la: LazyList[A], lb: LazyList[B]) =>
      (la, lb) match
        case (Empty, Empty)       => None
        case (Empty, Cons(b, bs)) => Some(None -> Some(b()), empty -> bs())
        case (Cons(a, as), Empty) => Some(Some(a()) -> None, as() -> empty)
        case (Cons(a, as), Cons(b, bs)) =>
          Some(Some(a()) -> Some(b()), as() -> bs())
    }

  def startsWith[B >: A](prefix: LazyList[B]): Boolean =
    zipAll(prefix).takeWhile(_(1).isDefined).forAll((a1, a2) => a1 == a2)

  def tails: LazyList[LazyList[A]] =
    unfold(this):
      case Empty      => None
      case Cons(h, t) => Some((t(), t()))
    .append(LazyList(empty))

  def hasSubsequence[B >: A](sub: LazyList[B]): Boolean =
    tails.exists(_.startsWith(sub))

  def scanRight[B](init: B)(f: (A, B) => B): LazyList[B] =
    foldRight(init -> LazyList(init)): (a, b0) =>
      lazy val b1 = b0
      val b2      = f(a, b1(0))
      (b2, cons(b2, b1(1)))
    ._2

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] =
    lazy val one: LazyList[Int] = cons(1, one)
    one

  def continually[A](a: A): LazyList[A] =
    lazy val single: LazyList[A] = cons(a, single)
    single

  def from(n: Int): LazyList[Int] =
    cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def loop(x1: Int, x2: Int): LazyList[Int] =
      cons(x1, loop(x2, x1 + x2))
    loop(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None         => empty

  lazy val fibsViaUnfold: LazyList[Int] =
    unfold(0 -> 1)((x1, x2) => Some((x1, x2 -> (x1 + x2))))

  def fromViaUnfold(n: Int): LazyList[Int] =
    unfold(n)(x => Some(x, x + 1))

  def continuallyViaUnfold[A](a: A): LazyList[A] =
    unfold(a)(_ => Some(a, a))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(1)(_ => Some(1, 1))
