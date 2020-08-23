package codes.quine.labo.parsers
package inlineparse
package internal

import scala.reflect.macros.blackbox

object Macros {
  def StringLiteral(c: blackbox.Context)(string: c.Expr[String])(ctx: c.Expr[Parsing[Any]]): c.Expr[Parsing[Unit]] = {
    import c.universe._

    val name = string.actualType match {
      case ConstantType(Constant(s: String)) => c.Expr[String](Literal(Constant("\"" + s + "\"")))
      case _                                 => c.Expr[String](Literal(Constant(s"StringLiteral(...)")))
    }

    reify {
      val string0 = string.splice
      val ctx0 = ctx.splice
      if (ctx0.input.startsWith(string0, ctx0.pos)) ctx0.unit(ctx0.pos + string0.length)
      else ctx0.unexpected(name.splice)
    }
  }

  def CharIn(c: blackbox.Context)(s: c.Expr[String])(ctx: c.Expr[Parsing[Any]]): c.Expr[Parsing[Unit]] = {
    import c.universe._

    val name = s.actualType match {
      case ConstantType(Constant(s: String)) => c.Expr[String](Literal(Constant("CharIn(\"" + s + "\")")))
      case _                                 => c.Expr[String](Literal(Constant(s"CharIn(...)")))
    }

    reify {
      val s0 = s.splice
      val ctx0 = ctx.splice
      if (ctx0.pos < ctx0.input.length && s0.contains(ctx0.input.charAt(ctx0.pos))) ctx0.unit(ctx0.pos + 1)
      else ctx0.unexpected(name.splice)
    }
  }

  def CharsWhileIn(
      c: blackbox.Context
  )(s: c.Expr[String], min: c.Expr[Int])(ctx0: c.Expr[Parsing[Any]]): c.Expr[Parsing[Unit]] = {
    import c.universe._

    val name = s.actualType match {
      case ConstantType(Constant(s: String)) => c.Expr[String](Literal(Constant("CharsWhileIn(\"" + s + "\")")))
      case _                                 => c.Expr[String](Literal(Constant(s"CharsWhileIn(...)")))
    }

    reify {
      val s0 = s.splice
      val ctx = ctx0.splice
      var n = 0
      while (ctx.pos < ctx.input.length && s0.contains(ctx.input.charAt(ctx.pos))) {
        ctx.pos += 1
        n += 1
      }
      if (min.splice <= n) ctx.unit()
      else ctx.unexpected(name.splice)
    }
  }

  def CharsWhileIn1(c: blackbox.Context)(s: c.Expr[String])(ctx0: c.Expr[Parsing[Any]]): c.Expr[Parsing[Unit]] = {
    import c.universe._
    CharsWhileIn(c)(s, c.Expr[Int](Literal(Constant(1))))(ctx0)
  }

  def StringLiteralEagerOps(
      c: blackbox.Context
  )(string: c.Expr[String])(ctx: c.Expr[Parsing[Any]]): c.Expr[EagerOps[Unit]] =
    c.universe.reify(new EagerOps(StringLiteral(c)(string)(ctx).splice))

  def StringLiteralLazyOps(
      c: blackbox.Context
  )(string: c.Expr[String])(ctx: c.Expr[Parsing[Any]]): c.Expr[LazyOps[Unit]] =
    c.universe.reify(new LazyOps(() => StringLiteral(c)(string)(ctx).splice))
}
