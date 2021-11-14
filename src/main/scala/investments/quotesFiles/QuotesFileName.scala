package sisgrana
package investments.quotesFiles

import investments.files.FileName
import investments.files.filters.{FilterFunction, HasFilesFilter}
import java.time.{LocalDate, Year => JYear, YearMonth => JYearMonth}

case class QuotesFileName(period: QuotesFileName.Period) extends FileName

object QuotesFileName extends HasFilesFilter[QuotesFileName] {
  sealed trait Period
  case class Year(year: JYear) extends Period
  case class Month(month: JYearMonth) extends Period
  case class Date(date: LocalDate) extends Period

  private val YearQuotesRegex = """COTAHIST_A(\d{4})\.TXT""".r
  private val MonthQuotesRegex = """COTAHIST_M(\d{2})(\d{4})\.TXT""".r
  private val DateQuotesRegex = """COTAHIST_D(\d{2})(\d{2})(\d{4}).TXT""".r

  override def FilterFunction: FilterFunction[QuotesFileName] = {
    case YearQuotesRegex(yearString) =>
      val year = JYear.parse(yearString)
      QuotesFileName(QuotesFileName.Year(year))
    case MonthQuotesRegex(monthString, yearString) =>
      val yearMonth = JYearMonth.parse(s"$yearString-$monthString")
      QuotesFileName(QuotesFileName.Month(yearMonth))
    case DateQuotesRegex(dayString, monthString, yearString) =>
      val date = LocalDate.parse(s"$yearString-$monthString-$dayString")
      QuotesFileName(QuotesFileName.Date(date))
  }
}
