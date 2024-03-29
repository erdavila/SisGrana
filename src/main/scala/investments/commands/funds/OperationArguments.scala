package sisgrana
package investments.commands.funds

import investments.commands.funds.operations.list.ListChunkMaker
import java.time.YearMonth

sealed trait OperationArguments

object OperationArguments {
  case class List(
    monthRange: MonthRange,
    printOptions: ListChunkMaker.Options,
    positiveFilters: Seq[String],
    negativeFilters: Seq[String],
  ) extends OperationArguments

  case class Init(month: YearMonth) extends OperationArguments

  case class EvolutionOf(
    fund: String,
    monthRange: MonthRange,
  ) extends OperationArguments

  case class GetPrices(month: YearMonth) extends OperationArguments

  case class Rewrite(month: YearMonth) extends OperationArguments
}
