package sisgrana
package investments.commands.funds

import java.time.{DayOfWeek, LocalDate, YearMonth}
import scala.math.Ordering.Implicits._

class DaysCounter(noPriceDates: Set[LocalDate]) {
  def count(from: LocalDate, to: LocalDate): Int =
    Iterator.iterate(from)(_.plusDays(1))
      .drop(1)
      .takeWhile(_ <= to)
      .count(!isNoPriceDate(_))

  def lastDateOfYearMonth(yearMonth: YearMonth): LocalDate =
    Iterator.iterate(yearMonth.atEndOfMonth())(_.minusDays(1))
      .find(!isNoPriceDate(_))
      .get

  def isNoPriceDate(date: LocalDate): Boolean =
    noPriceDates.contains(date) || isWeekendDate(date)

  private def isWeekendDate(date: LocalDate): Boolean =
    date.getDayOfWeek match {
      case DayOfWeek.SATURDAY | DayOfWeek.SUNDAY => true
      case _ => false
    }
}
