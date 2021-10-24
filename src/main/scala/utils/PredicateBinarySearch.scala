package sisgrana
package utils

import scala.annotation.tailrec

object PredicateBinarySearch {
  sealed abstract class Result(val found: Boolean) {
    def toOption: Option[Int] = toEither.toOption
    def toEither: Either[Int, Int] =
      this match {
        case Found(index) => Right(index)
        case NotFound(insertionIndex) => Left(insertionIndex)
      }
  }

  case class Found(index: Int) extends Result(true)
  case class NotFound(insertionIndex: Int) extends Result(false)

  sealed trait Action
  case object ContinueSearchingAfter extends Action
  case object ContinueSearchingBefore extends Action
  case object StopSearching extends Action

  def search[A: Ordering, B](value: A, sortedSeq: IndexedSeq[B])(access: B => A): Result =
    search(sortedSeq) { b =>
      val a = access(b)
      a ?<=> value
    }

  def search[A](sortedSeq: IndexedSeq[A])(predicate: A => Action): Result =
    search(sortedSeq.length) { index =>
      val value = sortedSeq(index)
      predicate(value)
    }

  def search(length: Int)(predicate: Int => Action): Result = {
    @tailrec
    def loop(minIndex: Int, maxIndex: Int): Result =
      if (minIndex <= maxIndex) {
        val i = (minIndex + maxIndex) / 2
        predicate(i) match {
          case ContinueSearchingAfter => loop(minIndex = i + 1, maxIndex)
          case ContinueSearchingBefore => loop(minIndex, maxIndex = i - 1)
          case StopSearching => Found(i)
        }
      } else {
        NotFound(minIndex)
      }

    loop(0, length - 1)
  }

  implicit class ActionOperator[A](private val a: A) extends AnyVal {
    def ?<=>(other: A)(implicit ord: Ordering[A]): Action = {
      import ord._
      if (a < other) ContinueSearchingAfter
      else if (a > other) ContinueSearchingBefore
      else StopSearching
    }
  }
}
