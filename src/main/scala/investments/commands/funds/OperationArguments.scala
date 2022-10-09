package sisgrana
package investments.commands.funds

import java.time.YearMonth

sealed trait OperationArguments

object OperationArguments {
  case class List(
    initialMonth: YearMonth,
    finalMonth: YearMonth,
    printOptions: ChunkMaker.Options,
    positiveFilters: Seq[String],
    negativeFilters: Seq[String],
  ) extends OperationArguments

  case class Init(month: YearMonth) extends OperationArguments
}
