package codes.quine.labo

import scala.annotation.tailrec
import scala.language.implicitConversions

package object stackparse {
  type P[+T] = Parser[T]

  def parse[T](input: String, parser: P[T]): Parsed[T] = {
    @tailrec def loop[R](act: Parsing.Action[R]): Parsed[T] =
      act match {
        case Parsing.Call(parser, p, k)                       => loop(parser.run(p, k))
        case Parsing.Success(value, p, _, _: Parsing.Done[T]) => Parsed.Success(value, p.pos)
        case Parsing.Success(value, p, cut, k)                => loop(k.succeed(value, p, cut))
        case Parsing.Failure(p, _, _: Parsing.Done[_])        => Parsed.Failure(p.errorMessage, p.errorPos)
        case Parsing.Failure(p, cut, k)                       => loop(k.fail(p, cut))
      }
    loop(parser.run(Parsing(input), new Parsing.Done[T]()))
  }

  implicit def Literal(s: String): P[Unit] = Parser.Literal(s)

  def CharIn(s: String): P[Unit] = Parser.CharIn(s)

  def CharPred(f: Char => Boolean): P[Unit] = Parser.CharPred(f)

  def CharsWhileIn(s: String, min: Int = 1): P[Unit] = Parser.CharsWhileIn(s, min)

  def CharsWhile(f: Char => Boolean, min: Int = 1): P[Unit] = Parser.CharsWhile(f, min)

  val Start: P[Unit] = Parser.Start

  val End: P[Unit] = Parser.End

  def &?[T](parser: P[T]): P[T] = Parser.LookAhead(parser)

  def &!(parser: P[Any]): P[Unit] = Parser.NegativeLookAhead(parser)

  def P[T](parser: => P[T])(implicit name: sourcecode.Name): P[T] = Parser.Delay(() => parser, name.value)

  def Pass[T](value: T): P[T] = Parser.Pass(value)

  def Fail(message: String): P[Nothing] = Parser.Fail(message)
}
