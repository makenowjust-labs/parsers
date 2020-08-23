package codes.quine.labo.parsers

import scala.annotation.tailrec
import scala.language.experimental.macros
import scala.language.implicitConversions

package object inlineparse {
  type Optioner[-A] = common.Optioner[A]
  val Optioner = common.Optioner
  type Repeater[-A] = common.Repeater[A]
  val Repeater = common.Repeater
  type Sequencer[-A, -B] = common.Sequencer[A, B]
  val Sequencer = common.Sequencer
  type Parsed[+A] = common.Parsed[A]
  val Parsed = common.Parsed

  type P[+T] = Parsing[T]

  def parse[T](input: String, parser: P[Any] => P[T]): Parsed[T] =
    parser(new Parsing(input)).toParsed

  @inline def P[T](parser: P[T]): P[T] = parser

  @inline def Pass(implicit ctx: P[Any]): P[Unit] =
    ctx.unit()

  @inline def Pass[T](value: T)(implicit ctx: P[Any]): P[T] =
    ctx.success(value)

  @inline def Fail(implicit ctx: P[Any]): P[Nothing] = Fail("fail")

  @inline def Fail(message: String)(implicit ctx: P[Any]): P[Nothing] =
    ctx.fail(message)

  @inline def &?[T](parser: => P[T])(implicit ctx0: P[Any]): P[T] = {
    val prevPos = ctx0.pos
    val ctx1 = parser
    if (ctx1.isSuccess) ctx1.success(ctx1.get, prevPos)
    else ctx1.coerce[T]
  }

  @inline def &![T](parser: => P[T])(implicit ctx0: P[Any]): P[Unit] = {
    val prevPos = ctx0.pos
    val prevErrorPos = ctx0.errorPos
    // Set `p.errorPos` as `Int.MaxValue` for preventing to override an error message.
    // Generally an error message in negative look-ahead is not useful.
    ctx0.errorPos = Int.MaxValue
    val ctx1 = parser
    ctx1.errorPos = prevErrorPos
    if (ctx1.isSuccess) ctx1.fail("negative look-ahead", prevPos)
    else ctx1.unit(prevPos)
  }

  @inline def Start(implicit ctx: P[Any]): P[Unit] =
    if (ctx.pos == 0) ctx.unit()
    else ctx.unexpected("Start")

  @inline def End(implicit ctx: P[Any]): P[Unit] =
    if (ctx.pos == ctx.input.length) ctx.unit(ctx.pos)
    else ctx.unexpected("End")

  implicit def StringLiteral(string: String)(implicit ctx: P[Any]): P[Unit] =
    macro internal.Macros.StringLiteral

  def CharPred(p: Char => Boolean)(implicit ctx: P[Any]): P[Unit] =
    if (ctx.pos < ctx.input.length && p(ctx.input.charAt(ctx.pos))) ctx.unit(ctx.pos + 1)
    else ctx.unexpected("CharPred(...)")

  def CharIn(s: String)(implicit ctx: P[Any]): P[Unit] =
    macro internal.Macros.CharIn

  @inline def CharsWhile(p: Char => Boolean, min: Int = 1)(implicit ctx: P[Any]): P[Unit] = {
    var n = 0
    while (ctx.pos < ctx.input.length && p(ctx.input.charAt(ctx.pos))) {
      ctx.pos += 1
      n += 1
    }
    if (min <= n) ctx.unit()
    else ctx.unexpected("CharsWhile(...)")
  }

  def CharsWhileIn(s: String, min: Int)(implicit ctx0: P[Any]): P[Unit] =
    macro internal.Macros.CharsWhileIn

  def CharsWhileIn(s: String)(implicit ctx0: P[Any]): P[Unit] =
    macro internal.Macros.CharsWhileIn1

  @inline def AnyChar(implicit ctx: P[Any]): P[Unit] =
    if (ctx.pos < ctx.input.length) ctx.unit(ctx.pos + 1)
    else ctx.unexpected("AnyChar")

  @inline def NoCut[T](p: => P[T])(implicit ctx0: P[Any]): P[T] = {
    val cut = ctx0.cut
    val ctx1 = p
    ctx1.cut = cut
    ctx1
  }

  @inline implicit def EagerOps[T](ctx1: P[T]): EagerOps[T] = new EagerOps(ctx1)

  @inline implicit def StringLiteralEagerOps(string: String)(implicit ctx: P[Any]): inlineparse.EagerOps[Unit] =
    macro internal.Macros.StringLiteralEagerOps

  final class EagerOps[T](val ctx1: P[T]) extends AnyVal {
    @inline def ~[U, V](parser2: => P[U])(implicit seq: Sequencer.Aux[T, U, V]): P[V] =
      if (ctx1.isSuccess) {
        val value1 = ctx1.get
        val cut1 = ctx1.cut
        ctx1.cut = false
        val ctx2 = parser2
        ctx2.cut = cut1 || ctx2.cut
        if (ctx2.isSuccess) ctx2.success(seq(value1, ctx2.get))
        else ctx2.coerce[V]
      } else ctx1.coerce[V]

    @inline def ~/[U, V](parser2: => P[U])(implicit seq: Sequencer.Aux[T, U, V]): P[V] = {
      if (ctx1.isSuccess) ctx1.cut = true
      this ~ parser2
    }

    @inline def / : P[T] = {
      if (ctx1.isSuccess) ctx1.cut = true
      ctx1
    }

    @inline def map[V](f: T => V): P[V] =
      if (ctx1.isSuccess) ctx1.success(f(ctx1.get))
      else ctx1.coerce[V]

    @inline def flatMap[V](f: T => P[V]): P[V] =
      if (ctx1.isSuccess) {
        val cut1 = ctx1.cut
        ctx1.cut = false
        val ctx2 = f(ctx1.get)
        ctx2.cut = cut1 || ctx2.cut
        ctx2
      } else ctx1.coerce[V]
  }

  @inline implicit def LazyOps[T](parser: => P[T]): LazyOps[T] = new LazyOps(() => parser)

  implicit def StringLiteralLazyOps(string: String)(implicit ctx: P[Any]): inlineparse.LazyOps[Unit] =
    macro internal.Macros.StringLiteralLazyOps

  final class LazyOps[T](val parser: () => P[T]) extends AnyVal {
    @inline def rep[V](implicit ctx0: P[Any], rep: Repeater.Aux[T, V]): P[V] = this.rep(min = 0)

    @inline def rep[V](min: Int, max: Int = Int.MaxValue)(implicit ctx0: P[Any], rep: Repeater.Aux[T, V]): P[V] = {
      @tailrec def loop(ctx0: P[Any], as: V, cut: Boolean, n: Int): P[V] =
        if (n == max) ctx0.success(as)
        else {
          val prevPos = ctx0.pos
          ctx0.cut = false
          val ctx1 = parser()
          if (ctx1.isSuccess && (n < min || prevPos < ctx1.pos))
            loop(ctx1, rep.append(as, ctx1.get), cut || ctx1.cut, n + 1)
          else if (min <= n && !ctx1.cut) ctx1.success(as, prevPos, cut)
          else {
            ctx1.cut = cut || ctx1.cut
            if (prevPos == ctx1.pos) ctx1.fail("null repeat") else ctx1.coerce[V]
          }
        }
      loop(ctx0, rep.empty, ctx0.cut, 0)
    }

    @inline def count[V](n: Int)(implicit ctx0: P[Any], rep: Repeater.Aux[T, V]): P[V] =
      this.rep(min = n, max = n)

    @inline def ?[V](implicit ctx0: P[Any], opt: Optioner.Aux[T, V]): P[V] = {
      val startPos = ctx0.pos
      val ctx1 = parser()
      if (ctx1.isSuccess) ctx1.success(opt.some(ctx1.get))
      else if (!ctx1.cut) ctx1.success(opt.none, startPos)
      else ctx1.coerce[V]
    }

    @inline def |[U >: T](parser2: => P[U])(implicit ctx0: P[Any]): P[U] = {
      val startPos = ctx0.pos
      val startValue = ctx0.value
      val ctx1 = parser()
      if (ctx1.isSuccess || ctx1.cut) ctx1
      else {
        ctx0.isSuccess = true
        ctx0.pos = startPos
        ctx0.value = startValue
        parser2
      }
    }

    @inline def filter(f: T => Boolean)(implicit ctx0: P[Any]): P[T] = {
      val prevPos = ctx0.pos
      val ctx1 = parser()
      if (ctx1.isSuccess && f(ctx1.get)) ctx1
      else ctx1.fail("filter", prevPos)
    }

    @inline def named(name: String)(implicit ctx0: P[Any]): P[T] = {
      val oldNamePos = ctx0.namePos
      val oldName = ctx0.name
      ctx0.namePos = ctx0.pos
      ctx0.name = name
      val ctx1 = parser()
      ctx1.namePos = oldNamePos
      ctx1.name = oldName
      ctx1
    }

    @inline def !(implicit ctx0: P[Any]): P[String] = {
      val startPos = ctx0.pos
      val ctx1 = parser()
      if (ctx1.isSuccess) ctx1.success(ctx1.input.slice(startPos, ctx1.pos))
      else ctx1.coerce[String]
    }
  }
}
