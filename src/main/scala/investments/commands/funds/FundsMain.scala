package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth
import utils.TextAligner

object FundsMain {
  // TODO: when listing multiple months, check if final recordSet from previous month matches initialRecordSet of current month

  def main(args: Array[String]): Unit = {
    val parsedArgs = ArgsParser.parse(args)
    val chunkMaker = new ChunkMaker(parsedArgs.printOptions)

    val months = Iterator.iterate(parsedArgs.initialMonth)(_.plusMonths(1))
      .takeWhile(month => !month.isAfter(parsedArgs.finalMonth))

    val chunks = months
      .map { month =>
        val statement = FundsMonthStatementFileReader.read(month)
        val filteredStatement = applyFilters(statement, parsedArgs.positiveFilters, parsedArgs.negativeFilters)
        val completeStatement = ensureLastDayOfMonth(filteredStatement, month)
        val (initialRecordSet, recordSets) = StatementProcessor.process(month, completeStatement)
        chunkMaker.makeChunks(month, initialRecordSet, recordSets)
      }
      .reduce(_ ++ _)

    TextAligner.alignAndRender(chunks)
      .foreach(println)
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
