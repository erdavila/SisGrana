package sisgrana
package investments.commands.funds

import java.time.{DayOfWeek, LocalDate}
import scala.math.Ordering.Implicits._

class DaysCounter(noPriceDates: Set[LocalDate]) {
  def count(from: LocalDate, to: LocalDate): Int = {
    def isWeekendDate(date: LocalDate): Boolean =
      date.getDayOfWeek match {
        case DayOfWeek.SATURDAY | DayOfWeek.SUNDAY => true
        case _ => false
      }

    Iterator.iterate(from)(_.plusDays(1))
      .drop(1)
      .takeWhile(_ <= to)
      .count(date => !noPriceDates.contains(date) && !isWeekendDate(date))
  }
}
