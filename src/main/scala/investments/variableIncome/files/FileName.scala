package sisgrana
package investments.variableIncome.files

import java.time.{LocalDate, Year => JYear, YearMonth => JYearMonth}

trait FileName

object FileName {
  case class QuotesFileName(period: QuotesFileName.Period) extends FileName

  object QuotesFileName {
    sealed trait Period
    case class Year(year: JYear) extends Period
    case class Month(month: JYearMonth) extends Period
    case class Date(date: LocalDate) extends Period
  }
}
