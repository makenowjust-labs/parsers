package codes.quine.labo.parsers
package funcparse

import common.Parsed

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
  sealed trait Step[+T] {
    def toParsed: Parsed[T]
  }

  final case class Success[T](value: T, p: Parsing, cut: Boolean) extends Step[T] {
    def toParsed: Parsed[T] = Parsed.Success(value, p.pos)
  }

  final case class Failure(p: Parsing, cut: Boolean) extends Step[Nothing] {
    def toParsed: Parsed[Nothing] = Parsed.Failure(p.errorMessage, p.errorPos)
  }
}
