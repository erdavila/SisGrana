package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth

object FundsMain {
  // TODO: when listing multiple months, check if final recordSet from previous month matches initialRecordSet of current month

  def main(args: Array[String]): Unit = {
    val parsedArgs = ArgsParser.parse(args)

    val statement = FundsMonthStatementFileReader.read(parsedArgs.month)
    val filteredStatement = applyFilters(statement, parsedArgs.positiveFilters, parsedArgs.negativeFilters)
    val completeStatement = ensureLastDayOfMonth(filteredStatement, parsedArgs.month)
    val (initialRecordSet, recordSets) = StatementProcessor.process(parsedArgs.month, completeStatement)

    val printer = new Printer(parsedArgs.printOptions)
    printer.printMonthRecordSets(parsedArgs.month, initialRecordSet, recordSets)
  }

  private def applyFilters(statement: FundsStatement, positive: Seq[String], negative: Seq[String]): FundsStatement = {
    def predicate(fund: String): Boolean =
      (positive.isEmpty || positive.exists(filter => fund.contains(filter))) && !negative.exists(filter => fund.contains(filter))

    statement
      .modify(_.initialEntries).using(_.view.filterKeys(predicate).toMap)
      .modify(_.entries.each).using(_.view.filterKeys(predicate).toMap)
  }

  private def ensureLastDayOfMonth(statement: FundsStatement, yearMonth: YearMonth): FundsStatement = {
    val daysCounter = new DaysCounter(statement.noPriceDates)
    val lastDate = daysCounter.lastDateOfYearMonth(yearMonth)

    statement
      .modify(_.entries)
      .usingIf(!statement.entries.contains(lastDate))(_ + (lastDate -> Map.empty))
  }
}
