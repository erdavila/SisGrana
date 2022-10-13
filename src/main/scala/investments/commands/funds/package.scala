package sisgrana
package investments.commands

package object funds {
  type Presence[+A] = Option[A]
  type Present[+A] = Some[A]
  val Present: Some.type = Some
  val Missing: None.type = None

  private[funds] def sumIfAny[A: Numeric](values: Iterable[A]): Option[A] =
    Option.when(values.nonEmpty)(values.sum)

  private[funds] def composeRatesIfAny(rates: Iterable[Double]): Option[Double] =
    Option.when(rates.nonEmpty) {
      val finalRatePlus1 = rates.tail.foldLeft(rates.head + 1) { (accumulatedRatePlus1, rate) =>
        accumulatedRatePlus1 * (1 + rate)
      }
      finalRatePlus1 - 1
    }

  implicit class PresenceMapOps[A](private val map: Map[String, Presence[A]]) extends AnyVal {
    def present: Map[String, A] = map.collect { case k -> Present(v) => k -> v }

    def keysForMissing: Set[String] = map.collect { case k -> Missing => k }.toSet

    def partitionByPresence: (Set[String], Map[String, A]) = {
      val (x, y) = map.partitionMap {
        case k -> Present(v) => Right(k -> v)
        case k -> Missing => Left(k)
      }
      (x.toSet, y.toMap)
    }
  }

  implicit class AnyOps[A](private val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }
}
