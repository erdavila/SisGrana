package sisgrana
package investments.commands.funds.operations.init

import investments.commands.funds.{DaysCounter, MonthStatementChunkMaker, OperationArguments, RecordSet, StatementProcessor}
import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.YearMonth
import utils.{Exit, TextAligner}

object InitOperation {
  def execute(args: OperationArguments.Init): Unit = {
    val filePath = FundsMonthStatementFileReader.terminalFilePath(args.month)
    val file = new File(filePath)
    if (file.exists()) {
      Exit.withErrorMessage { stream =>
        stream.println(s"O arquivo já existe: $filePath")
      }
    }

    val lastMonthEndRecordSet = readPreviousMonthEndPositionRecordSet(args.month)

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
