package sisgrana
package investments.commands.incomeRate

import investments.model.NonQuoteDate
import java.time
import java.time.{LocalDate, Year => JYear, YearMonth}
import scala.util.{Failure, Try}
import utils.{DateRange, quoted}

sealed abstract class Period(minDate: time.LocalDate, maxDate: time.LocalDate) {
  val dateRange: DateRange = {
    val adjustedMinDate = NonQuoteDate.adjustToQuoteDate(minDate)
    val adjustedMaxDate = NonQuoteDate.adjustToQuoteDate(maxDate)
    require(adjustedMinDate `isBefore` adjustedMaxDate)

    utils.DateRange(
      adjustedMinDate,
      adjustedMaxDate,
    )
  }
}

object Period {
  case class Year(year: time.Year)
    extends Period(minDateFor(year), maxDateFor(year))

  case class YearRange(beginYear: time.Year, endYear: time.Year)
    extends Period(minDateFor(beginYear), maxDateFor(endYear))

  case class Month(yearMonth: time.YearMonth)
    extends Period(minDateFor(yearMonth), maxDateFor(yearMonth))

  case class MonthRange(beginYearMonth: time.YearMonth, endYearMonth: time.YearMonth)
    extends Period(minDateFor(beginYearMonth), maxDateFor(endYearMonth))

  case class DateRange(beginDate: time.LocalDate, endDate: time.LocalDate)
    extends Period(beginDate, endDate)

  object DateRange {
    def apply(dr: utils.DateRange): DateRange = DateRange(dr.beginDate, dr.endDate)
  }

  def parse(string: String): Period = {
    val t = string match {
      case s"$begin:$end" =>
        Try {
          val beginYear = JYear.parse(begin)
          val endYear = JYear.parse(end)
          Period.YearRange(beginYear, endYear)
        } `recover` { _ =>
          val beginMonth = YearMonth.parse(begin)
          val endMonth = YearMonth.parse(end)
          Period.MonthRange(beginMonth, endMonth)
        } `recover` { _ =>
          val beginDate = LocalDate.parse(begin)
          val endDate = LocalDate.parse(end)
          Period.DateRange(beginDate, endDate)
        }

      case _ =>
        Try {
          val year = JYear.parse(string)
          Period.Year(year)
        } `recover` { _ =>
          val month = YearMonth.parse(string)
          Period.Month(month)
        }
    }

    t.recoverWith { _ =>
      Failure(new IllegalArgumentException(s"Período inválido: ${quoted(string)}"))
    }.get
  }

  private def minDateFor(year: time.Year): time.LocalDate =
    minDateFor(year.atMonth(time.Month.JANUARY))

  private def maxDateFor(year: time.Year): time.LocalDate =
    maxDateFor(year.atMonth(time.Month.DECEMBER))

  private def minDateFor(yearMonth: time.YearMonth): time.LocalDate =
    yearMonth.atDay(1).minusDays(1)

  private def maxDateFor(yearMonth: time.YearMonth): time.LocalDate =
    yearMonth.atEndOfMonth()
}
