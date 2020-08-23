package codes.quine.labo.parsers.stackparse

final case class Parsing(
    input: String,
    pos: Int = 0,
    message: Option[String] = None,
    expected: Seq[String] = Seq.empty,
    errorPos: Int = -1,
    name: String = "",
    namePos: Int = Int.MaxValue
) {
  def reset(pos: Int): Parsing = copy(pos = pos)

  def advance(n: Int): Parsing = copy(pos = pos + n)

  def named(name: String, pos: Int = this.pos): Parsing = copy(name = name, namePos = pos)

  def unexpected(name: String, pos: Int = this.pos): Parsing =
    if (namePos <= pos && name != this.name) unexpected(this.name, namePos)
    else if (errorPos < pos) copy(errorPos = pos, expected = Seq(name), message = None)
    else if (errorPos == pos) copy(expected = expected :+ name, message = None)
    else this

  def fail(message: String, pos: Int = this.pos): Parsing =
    if (namePos <= pos) unexpected(this.name, namePos)
    else if (errorPos < pos) copy(errorPos = pos, expected = Seq.empty, message = Some(message))
    else if (errorPos == pos && expected.isEmpty) copy(expected = Seq.empty, message = Some(message))
    else this

  def errorMessage: String =
    message.getOrElse(s"expected: ${expected.sorted.distinct.mkString(", ")}")
}

object Parsing {
  sealed trait Action[+R]
  final case class Call[T, R](parser: Parser[T], p: Parsing, k: Cont[T, R]) extends Action[R]
  final case class Success[T, +R](value: T, p: Parsing, cut: Boolean, k: Cont[T, R]) extends Action[R]
  final case class Failure[T, R](p: Parsing, cut: Boolean, k: Cont[T, R]) extends Action[R]

  sealed trait Cont[-T, +R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R]
    def fail(p: Parsing, cut: Boolean): Action[R]
  }

  final class Done[-T] extends Cont[T, Nothing] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[Nothing] = sys.error("Parsing.Done cannot call succeed.")
    def fail(p: Parsing, cut: Boolean): Action[Nothing] = sys.error("Parsing.Done cannot call fail.")
  }

  final class CaptureCont[R](val pos: Int, val cont: Cont[String, R]) extends Cont[Any, R] {
    def succeed(value: Any, p: Parsing, cut: Boolean): Action[R] = Success(p.input.slice(pos, p.pos), p, cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class SequenceCont1[T, U, V, R](
      val parser2: Parser[U],
      val seq: Sequencer.Aux[T, U, V],
      val cut0: Boolean,
      val cont: Cont[V, R]
  ) extends Cont[T, R] {
    def succeed(value1: T, p: Parsing, cut: Boolean): Action[R] =
      Call(parser2, p, new SequenceCont2(value1, seq, cut0 || cut, cont))
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class SequenceCont2[T, U, V, R](
      val value1: T,
      val seq: Sequencer.Aux[T, U, V],
      val cut1: Boolean,
      val cont: Cont[V, R]
  ) extends Cont[U, R] {
    def succeed(value2: U, p: Parsing, cut: Boolean): Action[R] = Success(seq(value1, value2), p, cut1 || cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut1 || cut, cont)
  }

  final class CutCont[T, R](val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(value, p, true, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class NoCutCont[T, R](val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(value, p, false, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, false, cont)
  }

  final class RepeatCont[T, V, R](
      val parser: Parser[T],
      val min: Int,
      val max: Int,
      val rep: Repeater.Aux[T, V],
      val pos: Int,
      val cut0: Boolean,
      val count: Int,
      val values: V,
      val cont: Cont[V, R]
  ) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] =
      if (p.pos == pos && min <= count) fail(p.fail("null repeat"), cut)
      else {
        val values1 = rep.append(values, value)
        val count1 = count + 1
        if (count1 == max) Success(values1, p, cut0 || cut, cont)
        else Call(parser, p, new RepeatCont(parser, min, max, rep, p.pos, cut0 || cut, count1, values1, cont))
      }

    def fail(p: Parsing, cut: Boolean): Action[R] =
      if (!cut && min <= count) Success(values, p.reset(pos), cut0, cont)
      else Failure(p, cut0 || cut, cont)
  }

  final class OptionalCont[T, V, R](val opt: Optioner.Aux[T, V], val pos: Int, val cont: Cont[V, R])
      extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(opt.some(value), p, cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] =
      if (cut) Failure(p, true, cont) else Success(opt.none, p.reset(pos), false, cont)
  }

  final class AlternativeCont[T, R](val parser2: Parser[T], val pos: Int, val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(value, p, cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] =
      if (cut) Failure(p, true, cont) else Call(parser2, p.reset(pos), cont)
  }

  final class LookAheadCont[T, R](val pos: Int, val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(value, p.reset(pos), cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class NegativeLookAheadCont[R](val pos: Int, val errorPos: Int, val message: String, val cont: Cont[Unit, R])
      extends Cont[Any, R] {
    def succeed(value: Any, p: Parsing, cut: Boolean): Action[R] =
      Failure(p.copy(errorPos = errorPos).fail(message, pos), cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Success((), p.copy(errorPos = errorPos).reset(pos), cut, cont)
  }

  final class MapCont[T, U, R](val f: T => U, val cont: Cont[U, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(f(value), p, cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class FlatMapCont1[T, U, R](val f: T => Parser[U], val cont: Cont[U, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Call(f(value), p, new FlatMapCont2(cut, cont))
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class FlatMapCont2[U, R](val cut1: Boolean, val cont: Cont[U, R]) extends Cont[U, R] {
    def succeed(value: U, p: Parsing, cut: Boolean): Action[R] = Success(value, p, cut1 || cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut1 || cut, cont)
  }

  final class FilterCont[T, R](val f: T => Boolean, val pos: Int, val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] =
      if (f(value)) Success(value, p, cut, cont) else Failure(p.fail("filter", pos), cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p, cut, cont)
  }

  final class NamedCont[T, R](val oldName: String, val oldNamePos: Int, val cont: Cont[T, R]) extends Cont[T, R] {
    def succeed(value: T, p: Parsing, cut: Boolean): Action[R] = Success(value, p.named(oldName, oldNamePos), cut, cont)
    def fail(p: Parsing, cut: Boolean): Action[R] = Failure(p.named(oldName, oldNamePos), cut, cont)
  }
}
