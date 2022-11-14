package sisgrana
package investments.commands.funds

import investments.commands.funds.operations.evolutionOf.EvolutionOfOperation
import investments.commands.funds.operations.getPrices.GetPricesOperation
import investments.commands.funds.operations.init.InitOperation
import investments.commands.funds.operations.list.ListOperation
import investments.commands.funds.operations.rewrite.RewriteOperation

object FundsMain {

  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case opArgs: OperationArguments.List => ListOperation.execute(opArgs)
      case opArgs: OperationArguments.Init => InitOperation.execute(opArgs)
      case opArgs: OperationArguments.EvolutionOf => EvolutionOfOperation.execute(opArgs)
      case opArgs: OperationArguments.GetPrices => GetPricesOperation.execute(opArgs)
      case opArgs: OperationArguments.Rewrite => RewriteOperation.execute(opArgs)
    }
}
