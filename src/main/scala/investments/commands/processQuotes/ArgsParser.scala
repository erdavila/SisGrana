package sisgrana
package investments.commands.processQuotes

import investments.ArgumentsParser
import investments.files.MultiLevelFilePath
import java.io.PrintStream
import scala.util.{Failure, Success, Try}

object ArgsParser extends ArgumentsParser[(OperationArguments, Seq[MultiLevelFilePath])] with ArgumentsParser.Utils {

  private val FindVariationsOperation = "find-variations"
  private val GetQuotesOperation = "get-quotes"

  override protected val spec: Parser[(OperationArguments, Seq[MultiLevelFilePath])] =
    for {
      operation <- takeNext
      opArgs <-
        operation match {
          case FindVariationsOperation => findVariationArgs
          case GetQuotesOperation => getQuotesArgs
          case _ => error(s"Operação inválida: $operation")
        }
      paths <- takeRemaining(atLeast = 1) $ toPaths
    } yield (opArgs, paths)

  private def findVariationArgs: Parser[OperationArguments.FindVariation] =
    for {
      asset <- takeNext
      minVariation <- takeNext $ toRatio
      _ <- takeNextIf(_ == Delimiter)
    } yield OperationArguments.FindVariation(asset, minVariation)

  private def getQuotesArgs: Parser[OperationArguments.GetQuotes] =
    for {
      asset <- takeNext
      dateRanges <- takeWhile(_ != Delimiter) $ toDateRanges
      _ <- discardNext
    } yield OperationArguments.GetQuotes(asset, dateRanges)

  private def toRatio(arg: String): Double =
    Try {
      arg match {
        case s"${str}%" => str.toDouble / 100.0
        case str => str.toDouble
      }
    } match {
      case Failure(exception) => error(s"Valor inválido: $arg", exception)
      case Success(minVariation) => minVariation
    }

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  $FindVariationsOperation ATIVO VARIAÇÃO-MÍNIMA [--] ARQUIVO-DE-COTAÇÕES.ZIP...")
    printStream.println(s"  $GetQuotesOperation ATIVO DATA... -- ARQUIVO-DE-COTAÇÕES.ZIP...")
    printStream.println()
    printStream.println("""VARIAÇÃO-MÍNIMA pode ser em formato percentual. Exemplos: "12.3%" ou "0.123"""")
    printStream.println("""DATA pode ser intervalo. Exemplo: "2021-10-15:2021-10-30"""")
  }
}
