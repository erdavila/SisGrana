package sisgrana
package investments.incomeRate

import investments.model.NonQuoteDate
import java.time
import utils.DateRange

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

  private def minDateFor(year: time.Year): time.LocalDate =
    minDateFor(year.atMonth(time.Month.JANUARY))

  private def maxDateFor(year: time.Year): time.LocalDate =
    maxDateFor(year.atMonth(time.Month.DECEMBER))

  private def minDateFor(yearMonth: time.YearMonth): time.LocalDate =
    yearMonth.atDay(1).minusDays(1)

  private def maxDateFor(yearMonth: time.YearMonth): time.LocalDate =
    yearMonth.atEndOfMonth()
}
