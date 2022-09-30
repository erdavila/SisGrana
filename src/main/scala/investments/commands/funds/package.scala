package sisgrana
package investments.commands

package object funds {
  private[funds] def sumIfAny[A: Numeric](values: Iterable[A]): Option[A] =
    Option.when(values.nonEmpty)(values.sum)

  private[funds] def composeRatesIfAny(rates: Iterable[Double]): Option[Double] =
    Option.when(rates.nonEmpty) {
      val finalRatePlus1 = rates.tail.foldLeft(rates.head + 1) { (accumulatedRatePlus1, rate) =>
        accumulatedRatePlus1 * (1 + rate)
      }
      finalRatePlus1 - 1
    }
}
