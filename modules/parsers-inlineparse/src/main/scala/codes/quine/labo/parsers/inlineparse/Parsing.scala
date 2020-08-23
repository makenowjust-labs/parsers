package codes.quine.labo.parsers.inlineparse

import scala.collection.mutable

final class Parsing[+T](
    val input: String,
    var pos: Int = 0,
    var isSuccess: Boolean = true,
    var value: Any = null,
    var cut: Boolean = false,
    var namePos: Int = -1,
    var name: String = "",
    var errorPos: Int = 0,
    var expectedSize: Int = 0,
    val expected: mutable.ArrayBuffer[String] = mutable.ArrayBuffer.empty,
    var message: String = ""
) {
  @inline def get: T = value.asInstanceOf[T]

  @inline def success[V](value: V, pos: Int = this.pos, cut: Boolean = this.cut): Parsing[V] = {
    isSuccess = true
    this.value = value
    this.pos = pos
    this.cut = cut
    this.asInstanceOf[Parsing[V]]
  }

  @inline def unit[V](pos: Int = this.pos): Parsing[Unit] =
    success((), pos)

  @inline def fail(message: String, pos: Int = this.pos): Parsing[Nothing] = {
    isSuccess = false
    if (namePos >= 0) appendExpected(name, namePos)
    else {
      if (errorPos == pos && expected.isEmpty) {
        this.message = message
      } else if (errorPos < pos) {
        expected.clear()
        this.message = message
        errorPos = pos
      }
    }
    this.asInstanceOf[Parsing[Nothing]]
  }

  @inline def unexpected(expected: String, pos: Int = this.pos): Parsing[Nothing] = {
    isSuccess = false
    if (namePos >= 0) appendExpected(name, namePos)
    else appendExpected(expected, pos)
    this.asInstanceOf[Parsing[Nothing]]
  }

  @inline private def appendExpected(name: String, pos: Int): Unit = {
    if (errorPos == pos) {
      this.message = ""
      if (expected.size == expectedSize) {
        expected.append(name)
      } else {
        expected(expectedSize) = name
      }
      expectedSize += 1
    } else if (errorPos < pos) {
      this.message = ""
      errorPos = pos
      expectedSize = 1
      if (expected.size >= 1) {
        expected(0) = name
      } else {
        expected.append(name)
      }
    }
  }

  @inline def coerce[V]: Parsing[V] = this.asInstanceOf[Parsing[V]]

  def toParsed: Parsed[T] =
    if (isSuccess) Parsed.Success(value.asInstanceOf[T], pos)
    else if (message.nonEmpty) Parsed.Failure(message, errorPos)
    else Parsed.Failure(s"expected: ${expected.take(expectedSize).toSeq.sorted.distinct.mkString(", ")}", errorPos)
}
