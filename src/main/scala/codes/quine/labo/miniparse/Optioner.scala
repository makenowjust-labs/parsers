package codes.quine.labo.miniparse

trait Optioner[T] {
  type Result

  def none: Result

  def some(x: T): Result
}

object Optioner extends LowPriorityOptioner {
  type Aux[T, R] = Optioner[T] {
    type Result = R
  }

  def apply[T](implicit opt: Optioner[T]): Aux[T, opt.Result] = opt

  implicit val unit: Aux[Unit, Unit] = new Optioner[Unit] {
    type Result = Unit
    def none: Unit = ()
    def some(x: Unit): Unit = ()
  }
}

trait LowPriorityOptioner {
  implicit def option[T]: Optioner.Aux[T, Option[T]] = OptionOptioner.asInstanceOf[Optioner.Aux[T, Option[T]]]

  private object OptionOptioner extends Optioner[Any] {
    type Result = Option[Any]
    def none = None
    def some(x: Any): Option[Any] = Some(x)
  }
}
