package sisgrana
package investments.fileTypes.fundsMonthStatement

import investments.files.FileName
import investments.files.filters.{FilterFunction, HasFilesFilter}
import java.time.YearMonth
import scala.util.matching.Regex

case class FundsMonthStatementFileName(yearMonth: YearMonth) extends FileName

object FundsMonthStatementFileName extends HasFilesFilter[FundsMonthStatementFileName] {
  private val Regex: Regex = """^(\d{4}-\d{2}) - FUNDS\.ssv$""".r

  override def FilterFunction: FilterFunction[FundsMonthStatementFileName] = {
    case Regex(yearMonthString) =>
      val yearMonth = YearMonth.parse(yearMonthString)
      FundsMonthStatementFileName(yearMonth)
  }
}
