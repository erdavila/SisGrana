package sisgrana
package investments.commands.funds

import investments.commands.funds.operations.init.InitOperation
import investments.commands.funds.operations.list.ListOperation
import investments.fileTypes.fundsMonthStatement._
import utils.TextAligner
import utils.Traversing._

object FundsMain {

  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case opArgs: OperationArguments.List => ListOperation.execute(opArgs)
      case opArgs: OperationArguments.Init => InitOperation.execute(opArgs)
      case opArgs: OperationArguments.EvolutionOf => evolutionOf(opArgs)
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
