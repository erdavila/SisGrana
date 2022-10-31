package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.YearMonth
import utils.Traversing._
import utils.{Exit, TextAligner}

object FundsMain {
  private case class MonthTurnFundData(sharePrice: Double, shareAmount: Option[BigDecimal])

  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case opArgs: OperationArguments.List => list(opArgs)
      case opArgs: OperationArguments.Init => init(opArgs)
    }

  private def list(opArgs: OperationArguments.List): Unit = {
    val chunkMaker = new ListChunkMaker(opArgs.printOptions)

    val months = Iterator.iterate(opArgs.initialMonth)(_.plusMonths(1))
      .takeWhile(month => !month.isAfter(opArgs.finalMonth))

    val (_, chunksAndLastAccumulatedRecordSets) = months
      .foldMapLeft(Option.empty[Map[String, MonthTurnFundData]]) { (previousMonthFinalData, month) =>
        val statement = FundsMonthStatementFileReader.read(month) |>
          (applyFilters(_, opArgs.positiveFilters, opArgs.negativeFilters)) |>
          (ensureLastDayOfMonth(_, month))
        val (initialRecordSet, recordSets) = StatementProcessor.process(month, statement)

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
      opArgs.initialMonth, opArgs.finalMonth,
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

  private def ensureLastDayOfMonth(statement: FundsStatement, yearMonth: YearMonth): FundsStatement = {
    val daysCounter = new DaysCounter(statement.noPriceDates)
    val lastDate = daysCounter.lastDateOfYearMonth(yearMonth)

    statement
      .modify(_.entries)
      .usingIf(!statement.entries.contains(lastDate))(_ + (lastDate -> Map.empty))
  }

  private def toMonthTurnData(previousPositionRecords: Map[String, Record.Position.Previous]): Map[String, MonthTurnFundData] =
    previousPositionRecords.view
      .filter(_._2.shareAmount.nonEmpty)
      .mapValues(previousPositionRecord => MonthTurnFundData(previousPositionRecord.sharePrice, previousPositionRecord.shareAmount))
      .toMap

  private def init(opArgs: OperationArguments.Init): Unit = {
    val filePath = FundsMonthStatementFileReader.terminalFilePath(opArgs.month)
    val file = new File(filePath)
    if (file.exists()) {
      Exit.withErrorMessage { stream =>
        stream.println(s"O arquivo já existe: $filePath")
      }
    }

    val lastMonthEndRecordSet = readPreviousMonthEndPositionRecordSet(opArgs.month)

    val initialEntries = lastMonthEndRecordSet.positionRecords
      .present
      .view
      .mapValues(positionRecord =>
        FundsStatement.InitialEntry(
          sharePrice = positionRecord.sharePrice,
          shareAmount = positionRecord.shareAmount.get,
          note = None,
        )
      )
      .toMap

    val statement = FundsStatement(
      initialEntries,
      entries = Map.empty,
      noPriceDates = Set.empty,
    )
    val rowsChunks = MonthStatementChunkMaker.makeChunks(statement)

    val writer = new PrintWriter(new FileOutputStream(filePath))
    for (row <- TextAligner.alignAndRender(rowsChunks)) {
      writer.println(row)
    }
    writer.close()

    println(s"O arquivo foi gravado: $filePath")
  }

  private def readPreviousMonthEndPositionRecordSet(month: YearMonth): RecordSet.Position = {
    val previousMonth = month.minusMonths(1)
    val statement = FundsMonthStatementFileReader.read(previousMonth)
    val (_, recordSets) = StatementProcessor.process(previousMonth, statement)

    val daysCounter = new DaysCounter(statement.noPriceDates)
    val lastDate = daysCounter.lastDateOfYearMonth(previousMonth)

    recordSets.lastOption match {
      case Some(recordSet) if recordSet.position.date == lastDate && !recordSet.position.missingData => recordSet.position
      case _ => Exit.withErrorMessage { stream =>
        stream.println(s"Faltam dados no último dia do mês anterior ($lastDate)")
      }
    }
  }
}
