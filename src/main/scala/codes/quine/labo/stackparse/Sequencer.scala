package codes.quine.labo.stackparse

trait Sequencer[-A, -B] {
  type Result

  def apply(a: A, b: B): Result
}

object Sequencer extends LowPrioritySequencer {
  type Aux[-A, -B, R] = Sequencer[A, B] {
    type Result = R
  }

  def apply[A, B](implicit seq: Sequencer[A, B]): Aux[A, B, seq.Result] = seq

  implicit val unit: Aux[Unit, Unit, Unit] = new Sequencer[Unit, Unit] {
    type Result = Unit
    def apply(a: Unit, b: Unit): Unit = ()
  }

  implicit def left[A]: Aux[A, Unit, A] = LeftSequencer.asInstanceOf[Aux[A, Unit, A]]

  private object LeftSequencer extends Sequencer[Any, Unit] {
    type Result = Any
    def apply(a: Any, b: Unit): Any = a
  }

  implicit def right[B]: Aux[Unit, B, B] = RightSequencer.asInstanceOf[Aux[Unit, B, B]]

  private object RightSequencer extends Sequencer[Unit, Any] {
    type Result = Any
    def apply(a: Unit, b: Any): Any = b
  }

  implicit def tuple3[A, B, C]: Aux[(A, B), C, (A, B, C)] = Tuple3Sequencer.asInstanceOf[Aux[(A, B), C, (A, B, C)]]

  private object Tuple3Sequencer extends Sequencer[(Any, Any), Any] {
    type Result = (Any, Any, Any)
    def apply(a: (Any, Any), b: Any): (Any, Any, Any) = (a._1, a._2, b)
  }

  implicit def tuple4[A, B, C, D]: Aux[(A, B, C), D, (A, B, C, D)] =
    Tuple3Sequencer.asInstanceOf[Aux[(A, B, C), D, (A, B, C, D)]]

  private object Tuple4Sequencer extends Sequencer[(Any, Any, Any), Any] {
    type Result = (Any, Any, Any, Any)
    def apply(a: (Any, Any, Any), b: Any): (Any, Any, Any, Any) = (a._1, a._2, a._3, b)
  }
}

private[stackparse] trait LowPrioritySequencer {
  implicit def tuple2[A, B]: Sequencer.Aux[A, B, (A, B)] = Tuple2Sequencer.asInstanceOf[Sequencer.Aux[A, B, (A, B)]]

  private object Tuple2Sequencer extends Sequencer[Any, Any] {
    type Result = (Any, Any)
    def apply(a: Any, b: Any): (Any, Any) = (a, b)
  }
}
