package sisgrana
package investments.commands.funds.operations.list

import com.softwaremill.quicklens._
import investments.commands.funds.{AnyOps, OperationArguments, Record, StatementProcessor}
import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import utils.TextAligner
import utils.Traversing._

object ListOperation {
  private case class MonthTurnFundData(sharePrice: Double, shareAmount: Option[BigDecimal])

  def execute(args: OperationArguments.List): Unit = {
    val chunkMaker = new ListChunkMaker(args.printOptions)

    val (_, chunksAndLastAccumulatedRecordSets) = args.monthRange.iterator
      .foldMapLeft(Option.empty[Map[String, MonthTurnFundData]]) { (previousMonthFinalData, month) =>
        val statement = FundsMonthStatementFileReader.read(month) |>
          (applyFilters(_, args.positiveFilters, args.negativeFilters))
        val (initialRecordSet, recordSets) = StatementProcessor.process(month, statement, ensureLastDayOfMonth = true)

        val monthInitialData = toMonthTurnData(initialRecordSet.positionRecords.present)
        val showWarning = previousMonthFinalData.exists(_ != monthInitialData)
        val initialRecordSetOpt = Option.when(previousMonthFinalData.isEmpty || showWarning) {
          initialRecordSet
        }

        val chunks = chunkMaker.makeChunks(month, showWarning, initialRecordSetOpt, recordSets)
        val monthFinalData = toMonthTurnData(recordSets.last.position.positionRecords.present)
        (Some(monthFinalData), (chunks, recordSets.last.accumulated))
      }

    val (chunks, lastAccumulatedRecordSets) = chunksAndLastAccumulatedRecordSets.unzip

    val monthRangeSummaryChunks = chunkMaker.makeMonthRangeSummaryChunks(
      args.monthRange,
      StatementProcessor.sumAccumulatedRecordSets(lastAccumulatedRecordSets),
    )

    TextAligner.alignAndRender(chunks.flatten.toSeq ++ monthRangeSummaryChunks)
      .foreach(println)
  }

  private def applyFilters(statement: FundsStatement, positive: Seq[String], negative: Seq[String]): FundsStatement = {
    def predicate(fund: String): Boolean =
      (positive.isEmpty || positive.exists(filter => fund.contains(filter))) && !negative.exists(filter => fund.contains(filter))

    statement
      .modify(_.initialEntries).using(_.view.filterKeys(predicate).toMap)
      .modify(_.entries.each).using(_.view.filterKeys(predicate).toMap)
  }

  private def toMonthTurnData(previousPositionRecords: Map[String, Record.Position.Previous]): Map[String, MonthTurnFundData] =
    previousPositionRecords.view
      .filter(_._2.shareAmount.nonEmpty)
      .mapValues(previousPositionRecord => MonthTurnFundData(previousPositionRecord.sharePrice, previousPositionRecord.shareAmount))
      .toMap
}
