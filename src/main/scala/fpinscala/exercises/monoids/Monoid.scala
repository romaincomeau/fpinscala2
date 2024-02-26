package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*
import fpinscala.exercises.testing.Gen
import fpinscala.exercises.testing.Gen.`**`
import fpinscala.exercises.testing.Prop
import fpinscala.exercises.testing.Prop.*

trait Monoid[A] extends Semigroup[A]:
  def empty: A

object Monoid:

  given string: Monoid[String] with
    def combine(a1: String, a2: String) = a1 + a2
    val empty                           = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty                             = Nil

  given intAddition: Monoid[Int] with
    def combine(x: Int, y: Int) = x + y
    val empty                   = 0

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(x: Int, y: Int) = x * y
    val empty                   = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x || y
    val empty                           = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x && y
    val empty                           = true

  def firstOptionMonoid[A]: Monoid[Option[A]] = new:
    def combine(x: Option[A], y: Option[A]) = x `orElse` y
    val empty                               = None

  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty                  = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f: A => A, g: A => A): A => A = f andThen g
    val empty                                 = a => a // identity function

  import fpinscala.exercises.testing.Gen
  import fpinscala.exercises.testing.Prop
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop
      .forAll(gen ** gen ** gen):
        case a ** b ** c =>
          m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
      .tag("associativity")
    val identity = Prop
      .forAll(gen): a =>
        m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
      .tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)((a: A) => (b: B) => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length <= 1 then as.headOption.map(f).getOrElse(m.empty)
    else
      val (l, r) = as.splitAt(as.size / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def combine(a1: Par[A], a2: Par[A]) = a1.map2(a2)(m.combine)
    val empty                           = Par.unit(m.empty)

  def parFoldMap[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    Par
      .parMap(as)(f)
      .flatMap: bs =>
        foldMapV(bs, par(m))(b => Par.lazyUnit(b))

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    val empty = WC.Stub("")
    def combine(wc1: WC, wc2: WC) = (wc1, wc2) match
      case (WC.Stub(a), WC.Stub(b))       => WC.Stub(a + b)
      case (WC.Stub(a), WC.Part(l, w, r)) => WC.Part(a + l, w, r)
      case (WC.Part(l, w, r), WC.Stub(a)) => WC.Part(l, w, r + a)
      case (WC.Part(l1, w1, r1), WC.Part(l2, w2, r2)) =>
        WC.Part(l1, w1 + (if (r1 + l2).isEmpty then 0 else 1) + w2, r2)

    def count(s: String): Int =
      def wc(c: Char): WC =
        if c.isWhitespace then WC.Part("", 0, "")
        else WC.Stub(c.toString)

      def unstub(s: String) = if s.isEmpty then 0 else 1

      foldMapV(s.toIndexedSeq, wcMonoid)(wc) match
        case WC.Stub(s)       => unstub(s)
        case WC.Part(l, w, r) => unstub(l) + w + unstub(r)
    end count

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)]
  with
    def combine(x: (A, B), y: (A, B)) =
      (ma.combine(x(0), y(0)), mb.combine(x(1), y(1)))
    val empty = (ma.empty, mb.empty)

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(empty): (acc, k) =>
        acc.updated(
          k,
          mv.combine(a.getOrElse(k, mv.empty), b.getOrElse(k, mv.empty))
        )
    val empty = Map()

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = a => mb.combine(f(a), g(a))
    val empty: A => B                 = a => mb.empty

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    import Foldable.given
    as.foldMap(a => Map(a -> 1))

end Monoid
