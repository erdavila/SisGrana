package sisgrana
package investments.commands.incomeRate

import investments.model.NonQuoteDate
import java.time
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, YearMonth, Year => JYear}
import scala.util.{Failure, Try}
import utils.{DateRange, quoted}

sealed abstract class Period(minDate: time.LocalDate, maxDate: time.LocalDate, val lengthInChronoUnits: Int) {
  val dateRange: DateRange = {
    val adjustedMinDate = NonQuoteDate.adjustToQuoteDate(minDate)
    val adjustedMaxDate = NonQuoteDate.adjustToQuoteDate(maxDate)
    require(adjustedMinDate `isBefore` adjustedMaxDate)

    utils.DateRange(
      adjustedMinDate,
      adjustedMaxDate,
    )
  }

  def offset(offset: Int): Period
}

object Period {
  case class Year(year: time.Year)
    extends Period(minDateFor(year), maxDateFor(year), 1)
  {
    override def offset(offset: Int): Year = Year(year.plusYears(offset))
  }

  case class YearRange(beginYear: time.Year, endYear: time.Year)
    extends Period(minDateFor(beginYear), maxDateFor(endYear), (ChronoUnit.YEARS.between(beginYear, endYear) + 1).toInt)
  {
    override def offset(offset: Int): YearRange = YearRange(beginYear.plusYears(offset), endYear.plusYears(offset))
  }

  case class Month(yearMonth: time.YearMonth)
    extends Period(minDateFor(yearMonth), maxDateFor(yearMonth), 1)
  {
    override def offset(offset: Int): Month = Month(yearMonth.plusMonths(offset))
  }

  case class MonthRange(beginYearMonth: time.YearMonth, endYearMonth: time.YearMonth)
    extends Period(minDateFor(beginYearMonth), maxDateFor(endYearMonth), (ChronoUnit.MONTHS.between(beginYearMonth, endYearMonth) + 1).toInt)
  {
    override def offset(offset: Int): MonthRange = MonthRange(beginYearMonth.plusMonths(offset), endYearMonth.plusMonths(offset))
  }

  case class DateRange(beginDate: time.LocalDate, endDate: time.LocalDate)
    extends Period(beginDate, endDate, ChronoUnit.DAYS.between(beginDate, endDate).toInt)
  {
    override def offset(offset: Int): DateRange = DateRange(beginDate.plusDays(offset), endDate.plusDays(offset))
  }

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

    t.recoverWith { e =>
      Failure(new IllegalArgumentException(s"Período inválido: ${quoted(string)}", e))
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
