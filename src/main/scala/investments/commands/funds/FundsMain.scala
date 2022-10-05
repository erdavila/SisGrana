package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth

object FundsMain {
  // TODO: when listing multiple months, check if final recordSet from previous month matches initialRecordSet of current month

  def main(args: Array[String]): Unit = {
    val yearMonth = YearMonth.parse(args(0))
    val statement = FundsMonthStatementFileReader.read(yearMonth)
    val completeStatement = ensureLastDayOfMonth(statement, yearMonth)

    val (initialRecordSet, recordSets) = StatementProcessor.process(yearMonth, completeStatement)

    val printer = new Printer(
      accumulated = args.contains("--accumulated"),
      totalsOnly = args.contains("--totals-only"),
    )
    printer.printMonthRecordSets(yearMonth, initialRecordSet, recordSets)
  }

  private def ensureLastDayOfMonth(statement: FundsStatement, yearMonth: YearMonth): FundsStatement = {
    val daysCounter = new DaysCounter(statement.noPriceDates)
    val lastDate = daysCounter.lastDateOfYearMonth(yearMonth)

    statement
      .modify(_.entries)
      .usingIf(!statement.entries.contains(lastDate))(_ + (lastDate -> Map.empty))
  }
}
