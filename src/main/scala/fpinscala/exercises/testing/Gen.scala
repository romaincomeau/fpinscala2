package fpinscala.exercises.testing

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

import scala.annotation.targetName

import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import fpinscala.exercises.state.*
import Gen.*
import Prop.*
import Prop.Result
import Prop.Result.*

/* The library developed in this chapter goes through several iterations. This
 * file is just the shell, which you can fill in and modify while working
 * through the chapter. */

opaque type Prop = (MaxSize, TestCases, RNG) => Result
object Prop:
  opaque type MaxSize = Int
  object MaxSize:
    extension (x: MaxSize) def toInt: Int = x
    def fromInt(x: Int): MaxSize          = x

  opaque type TestCases = Int

  object TestCases:
    extension (x: TestCases) def toInt: Int = x
    def fromInt(x: Int): TestCases          = x

  opaque type FailedCase = String
  object FailedCase:
    extension (f: FailedCase) def string: String = f
    def fromString(s: String): FailedCase        = s
  opaque type SuccessCount = Int

  enum Result:
    case Passed
    case Falsified(failure: FailedCase, successes: SuccessCount)
    case Proved

    def isFalsified: Boolean = this match
      case Passed | Proved => false
      case Falsified(_, _) => true

  def apply(f: (TestCases, RNG) => Result): Prop =
    (_, n, rng) => f(n, rng)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop: (n, rng) =>
    randomLazyList(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map:
        case (a, i) =>
          try if f(a) then Passed else Falsified(a.toString, i)
          catch case e: Exception => Falsified(buildMsg(a, e), i)
      .find(_.isFalsified)
      .getOrElse(Passed)

  @targetName("forAllSized")
  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    (max, n, rng) =>
      val casesPerSize = (n.toInt - 1) / max.toInt + 1
      val props: LazyList[Prop] =
        LazyList
          .from(0)
          .take((n.toInt min max.toInt) + 1)
          .map(i => forAll(g(i))(f))
      val prop: Prop =
        props
          .map[Prop](p => (max, n, rng) => p(max, casesPerSize, rng))
          .toList
          .reduce(_ && _)
      prop(max, n, rng)

  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def verify(p: => Boolean): Prop =
    (_, _, _) => if p then Passed else Falsified("()", 0)
  extension (self: Prop)
    @targetName("andProp")
    def &&(that: Prop): Prop = (max, n, rng) =>
      self.label("first")(max, n, rng) match
        case Result.Passed => that.label("second")(max, n, rng)
        case Result.Proved => that.label("second")(max, n, rng)
        case fail          => fail

    @targetName("orProp")
    def ||(that: Prop): Prop = (max, n, rng) =>
      self.label("first")(max, n, rng) match
        case Falsified(_, _) => that.label("second")(max, n, rng)
        case ok              => ok

    def label(msg: String): Prop = (max, n, rng) =>
      self(max, n, rng) match
        case Result.Falsified(e, c) => Falsified(s"$msg($e)", c)
        case x                      => x

    def tag(msg: String): Prop = label(msg)

    def check(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Result = self(maxSize, testCases, rng)

    def run(
        maxSize: MaxSize = 100,
        testCases: TestCases = 100,
        rng: RNG = RNG.Simple(System.currentTimeMillis)
    ): Unit =
      self(maxSize, testCases, rng) match
        case Falsified(msg, n) =>
          println(s"! Falsified after $n passed tests:\n $msg")
        case Passed => println(s"+ OK, passed $testCases tests.")
        case Proved => println(s"+ OK, proved property.")

  end extension

  val executor: ExecutorService = Executors.newCachedThreadPool

  val p1 = forAll(smallInt.list): l =>
    val max = l.max
    l.forall(max >= _)

  val p2 = forAll(smallInt.nonEmptyList): l =>
    val max = l.max
    l.forall(max >= _)

  val p3 = forAll(smallInt.list): l =>
    val ls      = l.sorted
    val ordered = l.isEmpty || ls.zip(ls.tail).forall((a, b) => a <= b)
    ordered && l.forall(ls.contains) && ls.forall(l.contains)

  val p4 = Prop.verify:
    val p  = Par.unit(1).map(_ + 1)
    val p2 = Par.unit(2)
    p.run(executor).get == p2.run(executor).get

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = p.map2(p2)(_ == _)

  val p5 = Prop.verify:
    equal(Par.unit(1).map(_ + 1), Par.unit(2)).run(executor).get

  val p6 = Prop.forAll(smallInt): i =>
    equal(Par.unit(i).map(_ + 1), Par.unit(i + 1)).run(executor).get

  val executors: Gen[ExecutorService] = weighted(
    choose(1, 4).map(Executors.newFixedThreadPool) -> .75,
    unit(Executors.newCachedThreadPool)            -> .25
  )

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g)((s, a) => f(a).run(s).get)

  def forAllPar_1[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(executors ** g):
      case s ** a => f(a).run(s).get

  def verifyPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  // Express the property about fork from chapter 7—that fork(x) == x.
  val gpy2: Gen[Par[Int]] =
    choose(-100, 100)
      .listOfN(choose(0, 20))
      .map(ys =>
        ys.foldLeft(Par.unit(0))((p, y) => Par.fork(p.map2(Par.unit(y))(_ + _)))
      )

  val forkProp = Prop.forAllPar(gpy2)(y => equal(Par.fork(y), y))

end Prop

opaque type Gen[+A] = State[RNG, A]

object Gen:

  val smallInt = Gen.choose(-10, 10)

  def unit[A](a: => A): Gen[A] =
    State.unit(a)

  def boolean: Gen[Boolean] =
    State(RNG.boolean)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if b then g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)
    State(RNG.double).flatMap(d => if d < g1Threshold then g1._1 else g2._1)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start))

  def both[A](ga: Gen[A]): Gen[(A, A)] =
    ga.map2(ga)((_, _))

  /**
   * Extensions for the Gen[A] data type
   */
  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State.flatMap(self)(f)

    def listOfN(g: Gen[Int]): Gen[List[A]] =
      g.flatMap(n => self.listOfN(n))

    def toOption: Gen[Option[A]] =
      self.map2(unit(()))((a, _) => Some(a))

    def map[B](f: A => B): Gen[B] =
      State.map(self)(f)

    def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
      State.map2(self)(gb)(f)

    @annotation.targetName("product")
    def **[B](gb: Gen[B]): Gen[(A, B)] =
      map2(gb)((_, _))

    def listOfN(n: Int): Gen[List[A]] =
      State.sequence(List.fill(n)(self))

    def unsized: SGen[A] = _ => self

    def list: SGen[List[A]] = (n: Int) => self.listOfN(n)

    def nonEmptyList: SGen[List[A]] = (n: Int) => self.listOfN(n.max(1))
  end extension

  object `**`:
    def unapply[A, B](p: (A, B)) = Some(p)

opaque type SGen[+A] = Int => Gen[A]

object SGen:
  def unit[A](a: A): SGen[A] = (i: Int) => Gen.unit(a)

  extension [A](self: SGen[A])
    def map[B](f: A => B): SGen[B] =
      n => self(n).map(f)

    def flatMap[B](f: A => SGen[B]): SGen[B] =
      n => self(n).flatMap(a => f(a)(n))
  end extension
