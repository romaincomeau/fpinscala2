// package fpinscala.exercises.parsing
//
// import scala.util.matching.Regex
//
// // case class Location(input: String, offset: Int):
// //   infix def advanceBy(n: Int): Location = this.copy(offset = offset + n)
// //
// //   def toError(msg: String): ParseError =
// //     ParseError(List((this, msg)))
//
// enum Result[+A]:
//   case Success(get: A, consummed: Int)
//   case Failure(get: ParseError)
//
// opaque type Parser[+A] = Location => Result[A]
//
// object Parser extends Parsers[Parser]:
//   import Result.Failure
//   import Result.Success
//
//   extension [A](pa: Parser[A])
//     def run(input: String): Either[ParseError, A] =
//       pa(Location(input, 0)) match
//         case Success(a, _) => Right(a)
//         case Failure(e)    => Left(e)
//
//     def flatMap[B](f: A => Parser[B]): Parser[B] =
//       s =>
//         pa(s) match
//           case Success(a, n) => f(a)(s.advanceBy( n))
//           case Failure(s)    => Failure(s)
//
//     def or(that: => Parser[A]): Parser[A] =
//       l => pa(l) match
//         case s @ Success(_, _) => s
//         case Failure(_)        => that(l)
//
//     def slice: Parser[String] = ???
//   end extension
//
//   def string(x: String): Parser[String] = s =>
//     if s.input.startsWith(x) then Success(x, x.length)
//     else Failure(s.toError(s"failed to parse $x"))
//
//   def regex(r: Regex): Parser[String] =
//     l =>
//       r.findPrefixOf(l.remaining) match
//         case None    => Failure(l.toError(s"regex $r"))
//         case Some(m) => Success(m, m.length)
//
//
