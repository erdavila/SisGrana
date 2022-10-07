package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth
import utils.TextAligner
import utils.Traversing._

object FundsMain {
  private case class MonthTurnFundData(sharePrice: Option[Double], shareAmount: Option[BigDecimal])

  def main(args: Array[String]): Unit = {
    val parsedArgs = ArgsParser.parse(args)
    val chunkMaker = new ChunkMaker(parsedArgs.printOptions)

    val months = Iterator.iterate(parsedArgs.initialMonth)(_.plusMonths(1))
      .takeWhile(month => !month.isAfter(parsedArgs.finalMonth))

    val (_, chunks) = months
      .foldFlatMapLeft(Option.empty[Map[String, MonthTurnFundData]]) { (previousMonthFinalData, month) =>
        val statement = FundsMonthStatementFileReader.read(month) |>
          (applyFilters(_, parsedArgs.positiveFilters, parsedArgs.negativeFilters)) |>
          (ensureLastDayOfMonth(_, month))
        val (initialRecordSet, recordSets) = StatementProcessor.process(month, statement)

        val monthInitialData = toMonthTurnData(initialRecordSet.records)
        val warning = Option.when(previousMonthFinalData.exists(_ != monthInitialData)) {
          "DADOS INICIAIS DIFEREM DOS DADOS FINAIS DO MÊS ANTERIOR"
        }

        val initialRecordSetOpt = Option.when(previousMonthFinalData.isEmpty || warning.isDefined) {
          initialRecordSet
        }

        val chunks = chunkMaker.makeChunks(month, warning, initialRecordSetOpt, recordSets)
        val monthFinalData = toMonthTurnData(recordSets.last.records)
        (Some(monthFinalData), chunks)
      }

    TextAligner.alignAndRender(chunks.toSeq)
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

  private def toMonthTurnData(records: Map[String, PreviousRecord]): Map[String, MonthTurnFundData] =
    records.view
      .filter(_._2.shareAmount.nonEmpty)
      .mapValues(record => MonthTurnFundData(record.sharePrice, record.shareAmount))
      .toMap

  private implicit class AnyOps[A](private val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }
}
