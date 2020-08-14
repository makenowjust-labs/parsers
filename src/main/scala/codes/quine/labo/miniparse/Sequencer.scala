package codes.quine.labo.miniparse

trait Sequencer[A, B] {
  type Result

  def apply(a: A, b: B): Result
}

object Sequencer extends LowPrioritySequencer {
  type Aux[A, B, R] = Sequencer[A, B] {
    type Result = R
  }

  def apply[A, B](implicit seq: Sequencer[A, B]): Aux[A, B, seq.Result] = seq

  implicit val unit: Aux[Unit, Unit, Unit] = new Sequencer[Unit, Unit] {
    type Result = Unit
    def apply(a: Unit, b: Unit): Unit = ()
  }

  implicit def left[T]: Aux[T, Unit, T] = LeftSequencer.asInstanceOf[Aux[T, Unit, T]]

  private object LeftSequencer extends Sequencer[Any, Unit] {
    type Result = Any
    def apply(a: Any, b: Unit): Any = a
  }

  implicit def right[T]: Aux[Unit, T, T] = RightSequencer.asInstanceOf[Aux[Unit, T, T]]

  private object RightSequencer extends Sequencer[Unit, Any] {
    type Result = Any
    def apply(a: Unit, b: Any): Any = b
  }

  implicit def tuple3[T1, T2, T3]: Aux[(T1, T2), T3, (T1, T2, T3)] =
    Tuple3Sequencer.asInstanceOf[Aux[(T1, T2), T3, (T1, T2, T3)]]

  private object Tuple3Sequencer extends Sequencer[(Any, Any), Any] {
    type Result = (Any, Any, Any)
    def apply(ab: (Any, Any), c: Any): (Any, Any, Any) = (ab._1, ab._2, c)
  }

  implicit def tuple4[T1, T2, T3, T4]: Aux[(T1, T2, T3), T4, (T1, T2, T3, T4)] =
    Tuple3Sequencer.asInstanceOf[Aux[(T1, T2, T3), T4, (T1, T2, T3, T4)]]

  private object Tuple4Sequencer extends Sequencer[(Any, Any, Any), Any] {
    type Result = (Any, Any, Any, Any)
    def apply(abc: (Any, Any, Any), d: Any): (Any, Any, Any, Any) = (abc._1, abc._2, abc._3, d)
  }
}

trait LowPrioritySequencer {
  implicit def tuple2[T1, T2]: Sequencer.Aux[T1, T2, (T1, T2)] =
    Tuple2Sequencer.asInstanceOf[Sequencer.Aux[T1, T2, (T1, T2)]]

  private object Tuple2Sequencer extends Sequencer[Any, Any] {
    type Result = (Any, Any)
    def apply(a: Any, b: Any): (Any, Any) = (a, b)
  }
}
