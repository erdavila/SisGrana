package sisgrana
package utils

abstract class IteratorWithState[A] extends Iterator[A] {
  trait State {
    def hasNext: (Boolean, State)
    def next(): (A, State)
  }

  protected var state: State

  override final def hasNext: Boolean =
    evaluate(_.hasNext)

  override final def next(): A =
    evaluate(_.next())

  private def evaluate[T](f: State => (T, State)): T = {
    val (result, newState) = f(state)
    state = newState
    result
  }
}
