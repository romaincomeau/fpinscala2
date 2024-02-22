package fpinscala.exercises.datastructures

import scala.annotation.tailrec

object Debug extends App:
  println(List.hasSubsequence(List(2, 2, 3), List(2, 3)))

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List: /* `List` companion object. Contains functions for creating and
   * working with lists. */
  def sum(ints: List[Int]): Int = ints match /* A function that uses pattern
     * matching to add up a list of integers */
    case Nil         => 0 // The sum of the empty list is 0.
    case Cons(x, xs) => x + sum(xs) /* The sum of a list starting with `x` is
       * `x` plus the sum of the rest of the list. */

  def product(doubles: List[Double]): Double = doubles match
    case Nil          => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs)  => x * product(xs)

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  @annotation.nowarn /* Scalac gives a hint here via a warning */
  val result = List(1, 2, 3, 4, 5) match
    case Cons(x, Cons(2, Cons(4, _)))          => x
    case Nil                                   => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t)                            => h + sum(t)
    case _                                     => 101

  def append[A](a1: List[A], a2: List[A]): List[A] = appendViaFoldRight(a1, a2)

  def foldRight[A, B](as: List[A], acc: B, f: (A, B) => B): B = /* Utility
     * functions */
    as match
      case Nil         => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def _foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z, (b, a) => f(a, b))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, (x, y) => x + y)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1.0, _ * _)

  def tail[A](l: List[A]): List[A] = l match
    case Nil        => sys.error("tail of empty list")
    case Cons(_, t) => t

  def setHead[A](l: List[A], h: A): List[A] = l match
    case Nil        => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match
    case Nil                 => Nil
    case Cons(h, t) if n > 0 => drop(t, n - 1)
    case _                   => l

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _                  => l

  def init[A](l: List[A]): List[A] = l match
    case Nil          => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))

  def init2[A](l: List[A]): List[A] =
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def go(cur: List[A]): List[A] = cur match
      case Nil          => sys.error("init of empty list")
      case Cons(_, Nil) => List(buf.toList*)
      case Cons(h, t)   => buf += h; go(t)
    go(l)

  def length[A](l: List[A]): Int =
    foldRight(l, 0, (_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], acc: B, f: (B, A) => B): B = l match
    case Nil        => acc
    case Cons(h, t) => foldLeft(t, f(acc, h), f)

  def sumViaFoldLeft(ns: List[Int]): Int =
    foldLeft(ns, 0, _ + _)

  def productViaFoldLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0, _ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0, (b, _) => b + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A](), (t, h) => Cons(h, t))

  def reverse_fright[A](l: List[A]): List[A] =
    _foldRight(l)(List[A]())(Cons(_, _))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    _foldRight(l)(r)((h, t) => Cons(h, t))

  private def append_via_foldLeft[A](l1: List[A], l2: List[A]): List[A] =
    foldLeft(l1, l2, (t, h) => Cons(h, t))

  def concat[A](l: List[List[A]]): List[A] =
    _foldRight(l)(List[A]())(appendViaFoldRight)

  def incrementEach(l: List[Int]): List[Int] = l match
    case Nil        => Nil
    case Cons(h, t) => Cons(h + 1, incrementEach(t))

  def doubleToString(l: List[Double]): List[String] = l match
    case Nil        => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))

  def map[A, B](l: List[A], f: A => B): List[B] =
    _foldRight(l)(List[B]())((h, t) => Cons(f(h), t))

  private def _map[A, B](l: List[A])(f: A => B): List[B] = l match
    case Nil              => Nil
    case Cons(head, tail) => Cons(f(head), _map(tail)(f))

  def filter[A](as: List[A], f: A => Boolean): List[A] =
    _foldRight(as)(Nil: List[A])((h, t) => if f(h) then Cons(h, t) else t)

  def filter_1[A](as: List[A])(f: A => Boolean): List[A] = as match
    case Nil => Nil
    case Cons(h, t) =>
      if f(h) then Cons(h, filter_1(t)(f))
      else filter_1(t)(f)
  end filter_1

  def filter_2[A](as: List[A])(f: A => Boolean): List[A] =
    reverse(foldLeft(as, List[A](), (t, h) => if f(h) then Cons(h, t) else t))

  def filter_3[A](as: List[A], f: A => Boolean): List[A] =
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @tailrec
    def go(cur: List[A]): List[A] = cur match
      case Nil => List(buf.toList*)
      case Cons(h, t) =>
        if f(h) then { buf += h; go(t) }
        else go(t)
    go(as)

  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    _foldRight(as)(Nil: List[B])((h, t) => append(f(h), t))

  def flatMap_1[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as, f))

  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match
    case (Nil, _)                     => Nil
    case (_, Nil)                     => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addPairwise(t1, t2))

  def zipWith[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] =
    @tailrec
    def loop(a: List[A], b: List[B], acc: List[C]): List[C] = (a, b) match
      case (Nil, _)                   => acc
      case (_, Nil)                   => acc
      case (Cons(a, as), Cons(b, bs)) => loop(as, bs, Cons(f(a, b), acc))
    loop(la, lb, Nil: List[C])

  def zipWith_1[A, B, C](la: List[A], lb: List[B])(f: (A, B) => C): List[C] =
    (la, lb) match
      case (Nil, _)                   => Nil
      case (_, Nil)                   => Nil
      case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith_1(as, bs)(f))

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def loop(root: List[A], rest: List[A], matching: Boolean): Boolean =
      (root, rest) match
        case (_, Nil)          => true
        case (Nil, Cons(_, _)) => false
        case (Cons(h1, t1), Cons(h2, t2)) =>
          if h1 == h2 then loop(t1, t2, true)
          else if !matching then loop(t1, rest, false)
          else loop(root, sub, false)
    loop(sup, sub, false)

  @tailrec
  private def hasSubsequence_1[A](sup: List[A], sub: List[A]): Boolean =
    sup match
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t)                => hasSubsequence_1(t, sub)

  @tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match
    case (_, Nil) => true
    case (Cons(h0, t0), Cons(h1, t1)) if h0 == h1 =>
      startsWith(t0, t1)
    case _ => false
