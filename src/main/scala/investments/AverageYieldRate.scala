package sisgrana
package investments

import scala.annotation.tailrec
import scala.math.{abs, pow}

object AverageYieldRate {
  type Day = Int

  case class Balance(value: Double, day: Day)
  case class Transfer(value: Double, day: Day)

  def estimate(initialBalance: Balance, transfers: Seq[Transfer], finalBalance: Balance): Rate = {
    require(initialBalance.value > 0)
    require(initialBalance.day < finalBalance.day)
    for (t <- transfers) {
      require(t.day > initialBalance.day)
      require(t.day <= finalBalance.day)
    }

    if (transfers.isEmpty) {
      Rate(
        value = (finalBalance.value - initialBalance.value) / initialBalance.value,
        days = finalBalance.day - initialBalance.day,
      )
    } else {
      @tailrec
      def loop(minBound: Double, maxBound: Double, count: Int): Double = {
        val estimation = (minBound + maxBound) / 2
        if (estimation == minBound || estimation == maxBound || count >= 100) {
          // An estimation with greater precision is not possible or iteration limit reached
          estimation
        } else {
          val diff = (
            initialBalance.value * pow(1 + estimation, finalBalance.day - initialBalance.day)
              + transfers.map(t => t.value * pow(1 + estimation, finalBalance.day - t.day)).sum
              - finalBalance.value
          )
          //println("%d\t%.4f%%\t%.4f%%\t%.4f%%\t%.3g".format(count, 100*minBound, 100*estimation, 100*maxBound, diff))

          val error = abs(diff)
          if (error < 1e-15) {
            estimation
          } else if (diff > 0) {
            loop(minBound, estimation, count + 1)
          } else {
            loop(estimation, maxBound, count + 1)
          }
        }
      }

      Rate(
        value = loop(-1.0, +1.0, 1),
        days = 1,
      )
    }
  }
}
