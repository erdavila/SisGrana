package sisgrana
package investments.commands.portfolio

import investments.ArgumentsParser
import investments.model.StockbrokerAsset
import java.io.PrintStream
import java.time.LocalDate

sealed trait OperationArguments

object OperationArguments {
  sealed trait List extends OperationArguments
  case object List extends List

  case class Show(portfolioName: String) extends OperationArguments

  case class Add(portfolioName: String, stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDate: Option[LocalDate]) extends OperationArguments

  case class Remove(portfolioName: String, stockbrokerAsset: StockbrokerAsset, beginDate: Option[LocalDate], endDate: Option[LocalDate]) extends OperationArguments
}

object ArgsParser extends ArgumentsParser[OperationArguments] with ArgumentsParser.Utils {
  private val ListOperation = "list"
  private val ShowOperation = "show"
  private val AddOperation = "add"
  private val RemoveOperation = "remove"

  override protected def spec: Parser[OperationArguments] =
    for {
      operation <- takeNext
      opArgs <- operation match {
        case ListOperation => takeListOperationArgs
        case ShowOperation => takeShowOperationArgs
        case AddOperation => takeAddOperationArgs
        case RemoveOperation => takeRemoveOperationArgs
        case _ => error(s"Operação inválida: $operation")
      }
    } yield opArgs

  private def takeListOperationArgs: Parser[OperationArguments.List] =
    const(OperationArguments.List)

  private def takeShowOperationArgs: Parser[OperationArguments.Show] =
    for (portfolioName <- takeNext)
      yield OperationArguments.Show(portfolioName)

  private def takeAddOperationArgs: Parser[OperationArguments.Add] =
    for {
      portfolioName <- takeNext
      stockbrokerAsset <- takeStockbrokerAsset
      beginDate <- takeDate
      endDate <- takeOptionalDate
    } yield OperationArguments.Add(portfolioName, stockbrokerAsset, beginDate, endDate)

  private def takeRemoveOperationArgs: Parser[OperationArguments.Remove] =
    for {
      portfolioName <- takeNext
      stockbrokerAsset <- takeStockbrokerAsset
      beginDate <- takeOptionalDate
      endDate <- takeOptionalDate
    } yield OperationArguments.Remove(portfolioName, stockbrokerAsset, beginDate, endDate)

  private def takeStockbrokerAsset: Parser[StockbrokerAsset] =
    for {
      str <- takeNext
      i = str.indexOf(':')
      (asset, stockbroker) = if (i >= 0) {
        (str.take(i), str.drop(i + 1))
      } else {
        error(s"ATIVO:CORRETORA inválido: $str")
      }
    } yield StockbrokerAsset(stockbroker, asset)

  private def takeDate: Parser[LocalDate] =
    takeNext $ toDate

  private def takeOptionalDate: Parser[Option[LocalDate]] =
    takeNextIfAny $$ toDate

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados:")
    printStream.println(s"  $ListOperation")
    printStream.println(s"  $ShowOperation CARTEIRA")
    printStream.println(s"  $AddOperation CARTEIRA ATIVO:CORRETORA DATA-INICIAL [DATA-FINAL]")
    printStream.println(s"  $RemoveOperation CARTEIRA ATIVO:CORRETORA [DATA-INICIAL [DATA-FINAL]]")
  }
}
