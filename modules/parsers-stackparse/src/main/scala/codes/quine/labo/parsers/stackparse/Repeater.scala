package codes.quine.labo.parsers.stackparse

trait Repeater[-A] {
  type Result

  def empty: Result
  def append(as: Result, a: A): Result
}

object Repeater extends LowPriorityRepeater {
  type Aux[-A, R] = Repeater[A] {
    type Result = R
  }

  def apply[A](implicit rep: Repeater[A]): Aux[A, rep.Result] = rep

  implicit val unit: Aux[Unit, Unit] = new Repeater[Unit] {
    type Result = Unit
    def empty: Unit = ()
    def append(as: Unit, a: Unit): Unit = ()
  }
}

private[stackparse] trait LowPriorityRepeater {
  implicit def seq[A]: Repeater.Aux[A, Seq[A]] = SeqRepeater.asInstanceOf[Repeater.Aux[A, Seq[A]]]

  object SeqRepeater extends Repeater[Any] {
    type Result = Seq[Any]
    def empty: Seq[Any] = Seq.empty[Any]
    def append(as: Seq[Any], a: Any): Seq[Any] = as :+ a
  }
}
