package sisgrana
package investments.commands.funds.operations.rewrite

import investments.commands.funds.{OperationArguments, StatementRewriter}

object RewriteOperation {
  def execute(args: OperationArguments.Rewrite): Unit = {
    val rewriter = new StatementRewriter(args.month)
    val statement = rewriter.read()
    rewriter.rewrite(statement)
  }
}
