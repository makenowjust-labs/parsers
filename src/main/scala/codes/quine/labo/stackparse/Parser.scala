package codes.quine.labo.stackparse

import scala.annotation.tailrec

sealed trait Parser[+T] {
  def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R]

  def ! : Parser[String] = Parser.Capture(this)

  def ~[U, V](parser2: Parser[U])(implicit seq: Sequencer.Aux[T, U, V]): Parser[V] =
    Parser.Sequence(this, parser2, false, seq)

  def ~/[U, V](parser2: Parser[U])(implicit seq: Sequencer.Aux[T, U, V]): Parser[V] =
    Parser.Sequence(this, parser2, true, seq)

  def / : Parser[T] = Parser.Cut(this)

  def rep[V](implicit rep: Repeater.Aux[T, V]): Parser[V] =
    Parser.Repeat(this, 0, Int.MaxValue, rep)

  def rep[V](min: Int, max: Int = Int.MaxValue)(implicit rep: Repeater.Aux[T, V]): Parser[V] =
    Parser.Repeat(this, min, max, rep)

  def count[V](times: Int)(implicit rep: Repeater.Aux[T, V]): Parser[V] =
    Parser.Count(this, times, rep)

  def ?[V](implicit opt: Optioner.Aux[T, V]): Parser[V] =
    Parser.Optional(this, opt)

  def |[U >: T](parser2: Parser[U]): Parser[U] =
    Parser.Alternative(this, parser2)

  def map[U](f: T => U): Parser[U] =
    Parser.Map(this, f)

  def flatMap[U](f: T => Parser[U]): Parser[U] =
    Parser.FlatMap(this, f)

  def filter(f: T => Boolean, message: String = "filter"): Parser[T] =
    Parser.Filter(this, f, message)

  def named(name: String): Parser[T] = Parser.Named(this, name)
}

object Parser {
  final case class Literal(string: String) extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit, R]): Parsing.Action[R] =
      if (p.input.startsWith(string, p.pos)) Parsing.Success((), p.advance(string.length), false, k)
      else Parsing.Failure(p.unexpected(name), false, k)

    lazy val name: String = "\"" + Util.escape(string) + "\""
    override def toString: String = name
  }

  final case class CharIn(string: String) extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit, R]): Parsing.Action[R] =
      if (p.pos < p.input.length && string.contains(p.input.charAt(p.pos))) Parsing.Success((), p.advance(1), false, k)
      else Parsing.Failure(p.unexpected(name), false, k)

    lazy val name: String = "CharIn(\"" + Util.escape(string) + "\")"
    override def toString: String = name
  }

  final case class CharPred(f: Char => Boolean) extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit, R]): Parsing.Action[R] =
      if (p.pos < p.input.length && f(p.input.charAt(p.pos))) Parsing.Success((), p.advance(1), false, k)
      else Parsing.Failure(p.unexpected(toString), false, k)

    override def toString: String = "CharPred(...)"
  }

  final case class CharsWhileIn(string: String, min: Int) extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit, R]): Parsing.Action[R] = {
      var pos = p.pos
      while (pos < p.input.length && string.contains(p.input.charAt(pos))) {
        pos += 1
      }
      if (min <= pos - p.pos) Parsing.Success((), p.reset(pos), false, k)
      else Parsing.Failure(p.unexpected(name), false, k)
    }

    lazy val name: String = "CharsWhileIn(\"" + Util.escape(string) + "\")"
    override def toString: String = name
  }

  final case class CharsWhile(f: Char => Boolean, min: Int) extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit, R]): Parsing.Action[R] = {
      var pos = p.pos
      while (pos < p.input.length && f(p.input.charAt(pos))) {
        pos += 1
      }
      if (min <= pos - p.pos) Parsing.Success((), p.reset(pos), false, k)
      else Parsing.Failure(p.unexpected(toString), false, k)
    }

    override def toString: String = "CharsWhile(...)"
  }

  case object Start extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit,R]): Parsing.Action[R] =
      if (p.pos == 0) Parsing.Success((), p, false, k)
      else Parsing.Failure(p.unexpected(toString), false, k)
  }

  case object End extends Parser[Unit] {
    def run[R](p: Parsing, k: Parsing.Cont[Unit,R]): Parsing.Action[R] =
      if (p.pos == p.input.length) Parsing.Success((), p, false, k)
      else Parsing.Failure(p.unexpected(toString), false, k)
  }

  final case class Capture(parser: Parser[Any]) extends Parser[String] {
    def run[R](p: Parsing, k: Parsing.Cont[String, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.CaptureCont(p.pos, k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).!"
        case _                                        => s"$parser.!"
      }
  }

  final case class Sequence[T, U, V](parser1: Parser[T], parser2: Parser[U], cut: Boolean, seq: Sequencer.Aux[T, U, V])
      extends Parser[V] {
    def run[R](p: Parsing, k: Parsing.Cont[V, R]): Parsing.Action[R] =
      Parsing.Call(parser1, p, new Parsing.SequenceCont1(parser2, seq, cut, k))

    override def toString: String = {
      def paren(p: Parser[Any]): String =
        p match {
          case _: Sequence[_, _, _] | _: Alternative[_] => "(" + p + ")"
          case _                                        => p.toString
        }
      @tailrec def loop(p1: Parser[Any], s: String, cut: Boolean): String =
        p1 match {
          case Sequence(p1, p2, _, _) => loop(p1, s"${paren(p2)} ${if (cut) "~/" else "~"} $s", cut)
          case _                      => s"${paren(p1)} ${if (cut) "~/" else "~"} $s"
        }
      loop(parser1, paren(parser2), cut)
    }
  }

  final case class Cut[T](parser: Parser[T]) extends Parser[T] {
    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.CutCont(k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser)./"
        case _                                        => s"$parser./"
      }
  }

  final case class Repeat[T, V](parser: Parser[T], min: Int, max: Int, rep: Repeater.Aux[T, V]) extends Parser[V] {
    def run[R](p: Parsing, k: Parsing.Cont[V, R]): Parsing.Action[R] =
      if (min == 0 && max == 0) Parsing.Success(rep.empty, p, false, k)
      else Parsing.Call(parser, p, new Parsing.RepeatCont(parser, min, max, rep, p.pos, false, 0, rep.empty, k))

    override def toString: String = {
      val method = (min, max) match {
        case (0, Int.MaxValue) => "rep"
        case (m, Int.MaxValue) => s"rep($m)"
        case (m, n)            => s"rep($m, $n)"
      }
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).$method"
        case _                                        => s"$parser.$method"
      }
    }
  }

  final case class Count[T, V](parser: Parser[T], times: Int, rep: Repeater.Aux[T, V]) extends Parser[V] {
    def run[R](p: Parsing, k: Parsing.Cont[V, R]): Parsing.Action[R] =
      if (times == 0) Parsing.Success(rep.empty, p, false, k)
      else Parsing.Call(parser, p, new Parsing.RepeatCont(parser, times, times, rep, p.pos, false, 0, rep.empty, k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).count($times)"
        case _                                        => s"$parser.count($times)"
      }
  }

  final case class Optional[T, V](parser: Parser[T], opt: Optioner.Aux[T, V]) extends Parser[V] {
    def run[R](p: Parsing, k: Parsing.Cont[V, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.OptionalCont(opt, p.pos, k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).?"
        case _                                        => s"$parser.?"
      }
  }

  final case class Alternative[T](parser1: Parser[T], parser2: Parser[T]) extends Parser[T] {
    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] =
      Parsing.Call(parser1, p, new Parsing.AlternativeCont(parser2, p.pos, k))

    override def toString: String = s"$parser1 | $parser2"
  }

  final case class Map[T, U](parser: Parser[T], f: T => U) extends Parser[U] {
    def run[R](p: Parsing, k: Parsing.Cont[U, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.MapCont(f, k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).map(...)"
        case _                                        => s"$parser.map(...)"
      }
  }

  final case class FlatMap[T, U](parser: Parser[T], f: T => Parser[U]) extends Parser[U] {
    def run[R](p: Parsing, k: Parsing.Cont[U, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.FlatMapCont1(f, k))

    override def toString: String =
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).flatMap(...)"
        case _                                        => s"$parser.flatMap(...)"
      }
  }

  final case class Filter[T](parser: Parser[T], f: T => Boolean, message: String) extends Parser[T] {
    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, new Parsing.FilterCont(f, message, k))

    override def toString: String = {
      val args = if (message == "filter") "" else ", \"" + Util.escape(message) + "\""
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).filter(...$args)"
        case _                                        => s"$parser.filter(...$args)"
      }
    }
  }

  final case class Delay[T](f: () => Parser[T], name: String) extends Parser[T] {
    lazy val parser: Parser[T] = f()

    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] =
      Parsing.Call(parser, p, k)

    override def toString: String = name
  }

  final case class Named[T](parser: Parser[T], name: String) extends Parser[T] {
    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] =
      Parsing.Call(parser, p.named(name), new Parsing.NamedCont(p.name, p.namePos, k))

    override def toString: String = {
      val args = "\"" + Util.escape(name) + "\""
      parser match {
        case _: Sequence[_, _, _] | _: Alternative[_] => s"($parser).named($args)"
        case _                                        => s"$parser.named($args)"
      }
    }
  }

  final case class Pass[T](value: T) extends Parser[T] {
    def run[R](p: Parsing, k: Parsing.Cont[T, R]): Parsing.Action[R] = Parsing.Success(value, p, false, k)

    override def toString: String = if (value == ()) "Pass" else s"Pass($value)"
  }

  final case class Fail(message: String) extends Parser[Nothing] {
    def run[R](p: Parsing, k: Parsing.Cont[Nothing, R]): Parsing.Action[R] =
      Parsing.Failure[Nothing, R](p.fail(message), false, k)

    override def toString: String =
      if (message == "fail") "Fail" else "Fail(\"" + Util.escape(message) + "\")"
  }
}
