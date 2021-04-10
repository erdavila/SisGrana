package sisgrana
package investments.research

import investments.AverageYieldRate.{Balance, Day, Transfer}
import investments.{AverageYieldRate, Rate}
import scala.util.Random

/*
 * This experiment demonstrates that, for a given total transfers value, the
 * estimated average rate tends to get closer to the real average rate when
 * the number of transfers is lower.
 */
object NumberOfTransfers {
  private val DaysCount = 10
  private val Days = 0 until DaysCount
  private val Iterations = 1000

  def main(args: Array[String]): Unit = {
    val transfersCountOfBestResult = Seq.fill(Iterations)(iterate())
    val occurrencesPerTransfersCount = transfersCountOfBestResult.groupMapReduce(identity)(_ => 1)(_ + _)
    for ((transfersCount, occurrences) <- occurrencesPerTransfersCount.toSeq.sortBy(_._1)) {
      println(s"$transfersCount: ${100 * occurrences / Iterations.toDouble}%")
    }
  }

  private def iterate(): Int = {
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
    val totalTransferValue = 2000 * Random.nextDouble() - 1000

    val estimatedRatesByTransfersCount =
      for {
        transfersCount <- LazyList.range(1, DaysCount - 1)
        transfersDays <- generateTransfersDays(transfersCount)
        transfers = transfersDays.map(day => day -> totalTransferValue / transfersCount).toMap
        finalBalanceValue = Days.drop(1).foldLeft(initialBalanceValue) { (previousDayBalance, day) =>
          val dayRate = dailyRates(day)
          val dayInitialBalance = previousDayBalance * (1 + dayRate.value)
          val dayTransfer = transfers.getOrElse(day, 0.0)
          val dayFinalBalance = dayInitialBalance + dayTransfer
          dayFinalBalance
        }
        estimatedRate = AverageYieldRate.estimate(
          Balance(initialBalanceValue, Days.head),
          transfers.map { case (day, value) => Transfer(value, day) }.toSeq,
          Balance(finalBalanceValue, Days.last),
        ).convert(toDays = 1)
      } yield transfersCount -> estimatedRate

    import Ordering.Double.TotalOrdering
    val (bestCount, _) = estimatedRatesByTransfersCount.minBy { case (_, rate) =>
      math.abs(rate.value - intervalAvgDailyRate.value)
    }

    bestCount
  }

  private def generateTransfersDays(count: Int, from: Int = 1): LazyList[Set[Day]] =
    if (count == 0) {
      LazyList(Set.empty)
    } else {
      for {
        day <- LazyList.range(from, DaysCount - 1)
        moreDays <- generateTransfersDays(count - 1, from = day + 1)
      } yield moreDays + day
    }
}
