package codes.quine.labo.parsers

import scala.language.implicitConversions
import scala.util.control.TailCalls.done

package object contparse {
  type Optioner[-A] = common.Optioner[A]
  val Optioner = common.Optioner
  type Repeater[-A] = common.Repeater[A]
  val Repeater = common.Repeater
  type Sequencer[-A, -B] = common.Sequencer[A, B]
  val Sequencer = common.Sequencer
  type Parsed[+A] = common.Parsed[A]
  val Parsed = common.Parsed

  type P[+T] = Parser[T]

  def parse[T](input: String, parser: P[T]): Parsed[T] =
    parser
      .run(
        Parsing(input),
        (x, p, _) => done(Parsed.Success(x, p.pos)),
        (p, _) => done(Parsed.Failure(p.errorMessage, p.errorPos))
      )
      .result

  implicit def Literal(s: String): P[Unit] = Parser.Literal(s)

  def CharIn(s: String): P[Unit] = Parser.CharIn(s)

  def CharPred(f: Char => Boolean): P[Unit] = Parser.CharPred(f)

  def CharsWhileIn(s: String, min: Int = 1): P[Unit] = Parser.CharsWhileIn(s, min)

  def CharsWhile(f: Char => Boolean, min: Int = 1): P[Unit] = Parser.CharsWhile(f, min)

  val AnyChar: P[Unit] = Parser.AnyChar

  val Start: P[Unit] = Parser.Start

  val End: P[Unit] = Parser.End

  def NoCut[T](parser: P[T]): P[T] = Parser.NoCut(parser)

  def &?[T](parser: P[T]): P[T] = Parser.LookAhead(parser)

  def &!(parser: P[Any]): P[Unit] = Parser.NegativeLookAhead(parser)

  def P[T](parser: => P[T])(implicit name: sourcecode.Name): P[T] = Parser.Delay(() => parser, name.value)

  def Pass: P[Unit] = Parser.Pass(())

  def Pass[T](value: T): P[T] = Parser.Pass(value)

  def Fail: P[Nothing] = Parser.Fail("fail")

  def Fail(message: String): P[Nothing] = Parser.Fail(message)
}
