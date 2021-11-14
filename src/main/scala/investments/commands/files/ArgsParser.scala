package sisgrana
package investments.commands.files

import investments.ArgumentsParser
import java.io.PrintStream

sealed trait OperationArguments

object OperationArguments {
  case class Resolve(path: String) extends OperationArguments
}

object ArgsParser extends ArgumentsParser[OperationArguments] with ArgumentsParser.Utils {
  private val ResolveOperation = "resolve"

  override protected def spec: Parser[OperationArguments] =
    for {
      operation <- takeNext
      opArgs <-
        operation match {
          case ResolveOperation => resolveArgs
          case _ => error(s"Operação inválida: $operation")
        }
    } yield opArgs

  private def resolveArgs: Parser[OperationArguments.Resolve] =
    for (path <- takeNext)
      yield OperationArguments.Resolve(path)

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  $ResolveOperation CAMINHO")
  }
}
