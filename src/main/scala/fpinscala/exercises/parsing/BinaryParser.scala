package fpinscala.exercises.parsing

import scala.annotation.targetName

enum Result[+A]:
  case Success(get: A, length: Int)
  case Failure(msg: String, isCommitted: Boolean)

  def advanceSuccess(n: Int): Result[A] = this match
    case Success(a, m) => Success(a, n + m)
    case _             => this

private case class ParseState(input: IndexedSeq[Byte], offset: Int):
  def advanceBy(n: Int): ParseState =
    this.copy(offset = offset + n)

  def readableAt(pos: Int): Boolean =
    pos <= input.size - 1

  def readableBetween(low: Int, high: Int): Boolean =
    low >= 0 && low <= high && high <= input.size

opaque type BinaryParser[+A] = ParseState => Result[A]

object BinaryParser:
  import Result.*
  private val InsufficientLength = "Expected more bytes than input"
  private val IntByteCount       = 4
  private val ShortByteCount     = 2

  extension [A](pa: BinaryParser[A])
    def run(input: IndexedSeq[Byte]): Result[A] =
      pa(ParseState(input, 0))

    def flatMap[B](f: A => BinaryParser[B]): BinaryParser[B] =
      s =>
        pa(s) match
          case Success(a, n) =>
            f(a)(s.advanceBy(n))
              .advanceSuccess(n)
          case Failure(m, c) => Failure(m, c)

    def map[B](f: A => B): BinaryParser[B] =
      flatMap(f andThen succeed)

    infix def product[B](pb: => BinaryParser[B]): BinaryParser[(A, B)] =
      for
        a <- pa
        b <- pb
      yield (a, b)

    def **[B](pb: => BinaryParser[B]): BinaryParser[(A, B)] = product(pb)

    def map2[B, C](
        pb: => BinaryParser[B]
    )(f: (A, B) => C): BinaryParser[C] =
      for
        a <- pa
        b <- pb
      yield f(a, b)

    def label(msg: String): BinaryParser[A] = s =>
      pa(s) match
        case Failure(s, c) => Failure(s"$msg($s)", c)
        case x             => x

    @targetName("ignoreR")
    def <*[B](pb: => BinaryParser[B]): BinaryParser[A] =
      pa.map2(pb)((a, b) => a)

    @targetName("ignoreL")
    def *>[B](pb: => BinaryParser[B]): BinaryParser[B] =
      pa.map2(pb)((a, b) => b)

    infix def or(p2: => BinaryParser[A]): BinaryParser[A] = s =>
      pa(s) match
        case Failure(_, false) => p2(s)
        case r                 => r

    def |(p2: => BinaryParser[A]): BinaryParser[A] = pa.or(p2)

    def many: BinaryParser[List[A]] =
      pa.map2(pa.many)(_ :: _) or succeed(Nil)

    def none: BinaryParser[Option[A]] =
      succeed(None)

    def some: BinaryParser[Option[A]] =
      map(Some(_))

  end extension

  extension [A, B, C](p: BinaryParser[((A, B), C)])
    def unbiasL: BinaryParser[(A, B, C)] = s =>
      p(s) match
        case Success(((a, b), c), n) => Success((a, b, c), n)
        case Failure(e, c)           => Failure(e, c)

  def succeed[A](a: A): BinaryParser[A] = _ => Success(a, 0)

  def byte: BinaryParser[Byte] = s =>
    if s.readableAt(s.offset) then Success(s.input(s.offset), 1)
    else Failure(s"expected a byte at offset ${s.offset}", true)

  def byte(value: Byte): BinaryParser[Byte] = s =>
    if s.readableAt(s.offset) then
      val b = s.input(s.offset)
      if b == value then Success(b, 1)
      else Failure(s"byte $b not matching expected byte $value", true)
    else Failure(InsufficientLength, true)

  @main def p(): Unit =
    val pl = byte ** byte ** byte
    val pr = byte ** (byte ** byte)

    val input = IndexedSeq(1, 2, 3).map(_.toByte)
    println(pl.run(input))
    println(pr.run(input))
end BinaryParser
