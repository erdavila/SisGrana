package sisgrana
package investments.commands.funds

import investments.commands.funds.operations.list.ListOperation
import investments.fileTypes.fundsMonthStatement._
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.YearMonth
import utils.Traversing._
import utils.{Exit, TextAligner}

object FundsMain {

  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case opArgs: OperationArguments.List => ListOperation.execute(opArgs)
      case opArgs: OperationArguments.Init => init(opArgs)
      case opArgs: OperationArguments.EvolutionOf => evolutionOf(opArgs)
    }

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

  private def evolutionOf(opArgs: OperationArguments.EvolutionOf): Unit = {
    val evolutionItems = opArgs.monthRange.iterator
      .traverse(Option.empty[BigDecimal]) { (previousMonthFinalShareAmount, month) =>
        val statement = FundsMonthStatementFileReader.read(month)
        val initialEntry = statement.initialEntries.get(opArgs.fund)
        val initialShareAmount = initialEntry.map(_.shareAmount).getOrElse(BigDecimal(0))

        if (previousMonthFinalShareAmount.exists(_ != initialShareAmount)) {
          throw new Exception(s"$month: $MonthTurnDataDifferMessage")
        }

        val initialItem = for {
          initialEntry <- initialEntry
          if previousMonthFinalShareAmount.isEmpty
        } yield EvolutionChunkMaker.EvolutionItem.PreviousMonth(
          previousMonth = month.minusMonths(1),
          shareAmount = initialEntry.shareAmount,
        )

        val items = for {
          (date, entries) <- statement.entries.toSeq.sortBy { case (date, _) => date }
          entry <- entries.get(opArgs.fund)
          shareAmount <- entry.shareAmountChange
          if shareAmount != 0
        } yield EvolutionChunkMaker.EvolutionItem.CurrentMonth(date, shareAmount, entry.sharePrice)

        val finalShareAmount = (initialShareAmount +: items.map(_.shareAmount)).sum

        (Some(finalShareAmount), initialItem ++ items)
      }(_ => None)

    val chunks = EvolutionChunkMaker.makeChunks(evolutionItems.toSeq)

    for (row <- TextAligner.alignAndRender(chunks)) {
      println(row)
    }
  }
}
