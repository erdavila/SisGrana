package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth

object FundsMain {
  // TODO: warn when there is no sharePrice for a previousRecord with shareAmount > 0
  // TODO: warn when there is no no data for the last day of the month
  // TODO: when listing multiple months, check if final recordSet from previous month matches initialRecordSet of current month

  def main(args: Array[String]): Unit = {
    val yearMonth = YearMonth.parse(args(0))
    val statement = FundsMonthStatementFileReader.read(yearMonth)
    val (initialRecordSet, recordSets) = StatementProcessor.process(yearMonth, statement)

    val printer = new Printer(accumulated = args.lift(1).contains("--accumulated"))
    printer.printMonthRecordSets(yearMonth, initialRecordSet, recordSets)
  }
}
