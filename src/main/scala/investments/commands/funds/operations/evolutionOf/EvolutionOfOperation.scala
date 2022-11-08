package sisgrana
package investments.commands.funds.operations.evolutionOf

import investments.commands.funds.{MonthTurnDataDifferMessage, OperationArguments}
import investments.fileTypes.fundsMonthStatement.FundsMonthStatementFileReader
import utils.TextAligner
import utils.Traversing._

object EvolutionOfOperation {
  def execute(args: OperationArguments.EvolutionOf): Unit = {
    val evolutionItems = args.monthRange.iterator
      .traverse(Option.empty[BigDecimal]) { (previousMonthFinalShareAmount, month) =>
        val statement = FundsMonthStatementFileReader.read(month)
        val initialEntry = statement.initialEntries.get(args.fund)
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
          entry <- entries.get(args.fund)
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
