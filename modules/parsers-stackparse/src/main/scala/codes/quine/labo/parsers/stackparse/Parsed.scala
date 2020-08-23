package codes.quine.labo.parsers.stackparse

sealed trait Parsed[+T] {
  def isSuccess: Boolean
  def value: T
  def pos: Int
}

object Parsed {
  final case class Success[+T](value: T, pos: Int) extends Parsed[T] {
    def isSuccess: Boolean = true
  }

  final case class Failure(message: String, pos: Int) extends Parsed[Nothing] {
    def isSuccess: Boolean = false
    def value: Nothing = sys.error("Parsed.Failure has no result value.")
  }
}
