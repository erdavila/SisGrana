package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.YearMonth
import utils.TextAligner.Chunk
import utils.Traversing._
import utils.{BrNumber, Exit, TextAligner, quoted}

object FundsMain {
  private case class MonthTurnFundData(sharePrice: Option[Double], shareAmount: Option[BigDecimal])

  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case opArgs: OperationArguments.List => list(opArgs)
      case opArgs: OperationArguments.Init => init(opArgs)
    }

  private def list(opArgs: OperationArguments.List): Unit = {
    val chunkMaker = new ChunkMaker(opArgs.printOptions)

    val months = Iterator.iterate(opArgs.initialMonth)(_.plusMonths(1))
      .takeWhile(month => !month.isAfter(opArgs.finalMonth))

    val (_, chunksAndRecordSetsAccumulated) = months
      .foldFlatMapLeft(Option.empty[Map[String, MonthTurnFundData]]) { (previousMonthFinalData, month) =>
        val statement = FundsMonthStatementFileReader.read(month) |>
          (applyFilters(_, opArgs.positiveFilters, opArgs.negativeFilters)) |>
          (ensureLastDayOfMonth(_, month))
        val (initialRecordSet, recordSets, recordSetAccumulated) = StatementProcessor.process(month, statement)

        val monthInitialData = toMonthTurnData(initialRecordSet.records)
        val warning = Option.when(previousMonthFinalData.exists(_ != monthInitialData)) {
          "DADOS INICIAIS DIFEREM DOS DADOS FINAIS DO MÊS ANTERIOR"
        }

        val initialRecordSetOpt = Option.when(previousMonthFinalData.isEmpty || warning.isDefined) {
          initialRecordSet
        }

        val chunks = chunkMaker.makeChunks(month, warning, initialRecordSetOpt, recordSets, recordSetAccumulated)
        val monthFinalData = toMonthTurnData(recordSets.last.records)
        (Some(monthFinalData), Some((chunks, recordSetAccumulated)))
      }

    val (chunks, recordSetsAccumulated) = chunksAndRecordSetsAccumulated.unzip

    val monthRangeSummaryChunks = if (recordSetsAccumulated.sizeIs > 1) {
      val monthRangeRecordSetAccumulated = StatementProcessor.sumRecordSetsAccumulated(recordSetsAccumulated)
      chunkMaker.makeSummaryChunks(
        s"Meses de ${opArgs.initialMonth} a ${opArgs.finalMonth} (${monthRangeRecordSetAccumulated.days} dias)",
        monthRangeRecordSetAccumulated,
        months = recordSetsAccumulated.size,
        nameIndentationSize = 2,
      )
    } else {
      Seq.empty
    }

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

  private def toMonthTurnData(records: Map[String, PreviousRecord]): Map[String, MonthTurnFundData] =
    records.view
      .filter(_._2.shareAmount.nonEmpty)
      .mapValues(record => MonthTurnFundData(record.sharePrice, record.shareAmount))
      .toMap

  private def init(opArgs: OperationArguments.Init): Unit = {
    val filePath = FundsMonthStatementFileReader.terminalFilePath(opArgs.month)
    val file = new File(filePath)
    if (file.exists()) {
      Exit.withErrorMessage { stream =>
        stream.println(s"O arquivo já existe: $filePath")
      }
    }

    val lastMonthEndRecordSet = readPreviousMonthEndRecordSet(opArgs.month)

    val Separator = "  "
    val headerRowChunks = Seq(
      Chunk.leftAligned(0, s"# Day${Separator}Fund"),
      Chunk.leftAligned(1, Separator),
      Chunk.rightAligned(2, s"Share Price${Separator}"),
      Chunk.rightAligned(3, s"Share Change"),
      Chunk.leftAligned(3, s"${Separator}Note"),
    )

    val nf = BrNumber.modifyNumberFormat { nf =>
      nf.setMinimumFractionDigits(8)
      nf.setMaximumFractionDigits(8)
    }
    val fundsRowsChunks = lastMonthEndRecordSet.records
      .toSeq
      .sortBy { case (fund, _) => fund }
      .map { case (fund, record) =>
        Seq(
          Chunk.leftAligned(0, s"INI${Separator}${quoted(fund)}"),
          Chunk.leftAligned(1, Separator),
          Chunk.rightAligned(2, s"${nf.format(record.sharePrice.get)}${Separator}"),
          Chunk.rightAligned(3, s"${nf.format(record.shareAmount.get.toDouble)}"),
        )
      }

    val rowsChunks = headerRowChunks +: Seq.empty +: fundsRowsChunks
    val writer = new PrintWriter(new FileOutputStream(filePath))
    for (row <- TextAligner.alignAndRender(rowsChunks)) {
      writer.println(row)
    }
    writer.close()

    println(s"O arquivo foi gravado: $filePath")
  }

  private def readPreviousMonthEndRecordSet(month: YearMonth): RecordSet = {
    val previousMonth = month.minusMonths(1)
    val statement = FundsMonthStatementFileReader.read(previousMonth)
    val (_, recordSets, _) = StatementProcessor.process(previousMonth, statement)

    val daysCounter = new DaysCounter(statement.noPriceDates)
    val lastDate = daysCounter.lastDateOfYearMonth(previousMonth)

    recordSets.lastOption match {
      case Some(recordSet) if recordSet.date == lastDate && recordSet.records.forall(!_._2.missingData) => recordSet
      case _ => Exit.withErrorMessage { stream =>
        stream.println(s"Faltam dados no último dia do mês anterior ($lastDate)")
      }
    }
  }

  private implicit class AnyOps[A](private val a: A) extends AnyVal {
    def |>[B](f: A => B): B = f(a)
  }
}
