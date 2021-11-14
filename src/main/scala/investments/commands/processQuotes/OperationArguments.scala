package sisgrana
package investments.commands.processQuotes

import utils.DateRanges

sealed trait OperationArguments

object OperationArguments {
  case class FindVariation(asset: String, minVariation: Double) extends OperationArguments
  case class GetQuotes(asset: String, dateRanges: DateRanges) extends OperationArguments
}
