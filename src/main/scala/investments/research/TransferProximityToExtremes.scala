package sisgrana
package investments.research

import investments.AverageYieldRate.{Balance, Day, Transfer}
import investments.{AverageYieldRate, Rate}
import scala.util.Random

/*
 * This experiment demonstrates that, on a interval with one transfer of a
 * given value, the estimated average rate tends to get closer to the real
 * average rate when the transfer day gets closer to the interval extremes.
 */
object TransferProximityToExtremes {
  private val Iterations: Int = 1000
  private val DaysCount: Int = 31

  private val Days = 0 until DaysCount

  def main(args: Array[String]): Unit = {
    val daysOfBestResult = Seq.fill(Iterations)(iterate())
    val occurrencesPerDay = daysOfBestResult.groupMapReduce(identity)(_ => 1)(_ + _)
    for ((day, occurrences) <- occurrencesPerDay.toSeq.sortBy(_._1)) {
      println(s"$day: ${100 * occurrences / Iterations.toDouble}%")
    }
  }

  private def iterate(): Day = {
    val dailyRates = IndexedSeq.iterate(Rate(0.0, 1), DaysCount) { r =>
      val rnd1 = 2 * Random.nextDouble() - 1
      val rnd2 = 2 * Random.nextDouble() - 1
      Rate(
        value = r.value + rnd1 * rnd2 * 0.01,
        days = 1,
      )
    }
    val intervalAvgDailyRate = dailyRates.reduce(_ `compose` _).convert(toDays = 1)

    val initialBalanceValue = 2000 * Random.nextDouble()
    val transferValue = 2000 * Random.nextDouble() - 1000

    val estimatedRatesByTransferDay =
      for {
        transferDay <- Days.drop(1).dropRight(1)
        finalBalanceValue = Days.drop(1).foldLeft(initialBalanceValue) { (previousDayBalance, day) =>
          val dayRate = dailyRates(day)
          val dayInitialBalance = previousDayBalance * (1 + dayRate.value)
          val dayTransfer = if (day == transferDay) transferValue else 0.0
          val dayFinalBalance = dayInitialBalance + dayTransfer
          dayFinalBalance
        }
        estimatedRate = AverageYieldRate.estimate(
          Balance(initialBalanceValue, Days.head),
          Seq(Transfer(transferValue, transferDay)),
          Balance(finalBalanceValue, Days.last),
        ).convert(toDays = 1)
      } yield transferDay -> estimatedRate

    import Ordering.Double.TotalOrdering
    val (bestDay, _) = estimatedRatesByTransferDay.minBy { case (_, rate) =>
      math.abs(rate.value - intervalAvgDailyRate.value)
    }

    bestDay
  }
}
