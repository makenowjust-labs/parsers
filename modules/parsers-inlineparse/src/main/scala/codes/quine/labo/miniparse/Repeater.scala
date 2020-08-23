package codes.quine.labo.miniparse

import scala.collection.mutable

trait Repeater[T] {
  type Builder

  type Result

  def builder: Builder

  def append(b: Builder, x: T): Unit

  def result(b: Builder): Result
}

object Repeater extends LowPriorityRepeater {
  type Aux[T, R] = Repeater[T] {
    type Result = R
  }

  def apply[T](implicit rep: Repeater[T]): Aux[T, rep.Result] = rep

  implicit val unit: Aux[Unit, Unit] = new Repeater[Unit] {
    type Builder = Unit
    type Result = Unit
    def builder: Unit = ()
    def append(b: Unit, x: Unit): Unit = ()
    def result(b: Unit): Unit = ()
  }
}

trait LowPriorityRepeater {
  implicit def seq[T]: Repeater.Aux[T, Seq[T]] = SeqRepeater.asInstanceOf[Repeater.Aux[T, Seq[T]]]

  private object SeqRepeater extends Repeater[Any] {
    type Builder = mutable.Builder[Any, Seq[Any]]
    type Result = Seq[Any]
    def builder: mutable.Builder[Any, Seq[Any]] = Seq.newBuilder[Any]
    def append(b: mutable.Builder[Any, Seq[Any]], x: Any): Unit = b.addOne(x)
    def result(b: mutable.Builder[Any, Seq[Any]]): Seq[Any] = b.result()
  }
}
