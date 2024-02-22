package fpinscala.exercises.parsing

import scala.annotation.targetName
import scala.util.matching.Regex

import fpinscala.answers.testing.exhaustive.Gen
import fpinscala.answers.testing.exhaustive.Prop

trait Parsers[Parser[+_]]:
  self => // so inner classes may call methods of trait

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]

    def map[B](f: A => B): Parser[B] =
      flatMap(f.andThen(pure))

    def flatMap[B](f: A => Parser[B]): Parser[B]

    private def product[B](pb: => Parser[B]): Parser[(A, B)] =
      for
        a <- p
        b <- pb
      yield (a, b)

    private def map2[B, C](pb: => Parser[B])(
        f: (A, B) => C
    ): Parser[C] = for
      a <- p
      b <- pb
    yield f(a, b)

    infix def or(p2: => Parser[A]): Parser[A]

    def as[B](b: B): Parser[B] =
      p.map(_ => b)

    def *>[B](p2: => Parser[B]) =
      p.slice.map2(p2)((a, b) => b)

    def <*[B](p2: => Parser[B]) =
      p.slice.map2(p2)((a, b) => a)

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

      /* Hard: Using map2 and succeed, implement the listOfN combinator from
       * earlier: */
    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def label(s: String): Parser[A] = ???

    def slice: Parser[String]

    def **[B](pb: => Parser[B]): Parser[(A, B)] = p.product(pb)

    def |(p2: => Parser[A]): Parser[A] = p.or(p2)
  end extension

  /* With many1, we can now implement the parser for zero or more 'a' followed
   * by one or more 'b', as follows: */

  def regex(r: Regex): Parser[String]

  def whitespace: Parser[String] = regex("\\s*".r)

  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  def string(s: String): Parser[String]

  def count[A](p: Parser[A]): Parser[Int] =
    p.many.slice.map(_.length)

  def one[A](p: Parser[A]): Parser[Int] =
    p.flatMap(_ => count(p))

  def pure[A](a: A): Parser[A] =
    string("").map(_ => a)

  // _alias_ for pure
  def succeed[A](a: A): Parser[A] = pure(a)

  case class ParserOps[A](p: Parser[A])

  object Laws:
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      Prop.forAll(in)(s => p1.run(s) == p2.run(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    private val l1 = (c: Char) => char(c).run(c.toString) == Right(c)
    private val l2 = (s: String) => string(s).run(s) == Right(s)
    private val l3 = (s: String) =>
      (n: Int) => count(string(s)).run(s.repeat(n)) == Right(n)

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col  = input.slice(0, offset + 1).reverse.indexOf('\n')

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)

  def slice(n: Int) = ???

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if (input.length > 1) input.linesIterator.drop(line - 1).next()
    else ""

case class ParseError(
    stack: List[(Location, String)] = List(),
    otherFailures: List[ParseError] = List()
):
  def push(loc: Location, msg: String): ParseError = ???

  def label(s: String): ParseError = ???

class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] = ???

  val nConsecutiveAs: Parser[Int] = ???
