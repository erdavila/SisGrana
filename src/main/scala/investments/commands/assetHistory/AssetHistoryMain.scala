package sisgrana
package investments.commands.assetHistory

import investments.Formatting
import investments.model.LocalDateSupport._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import investments.model.{AssetPeriod, StockbrokerAsset, ctx}
import java.time.LocalDate
import scala.util.{Failure, Success, Try}
import utils.IndentedPrinter

object AssetHistoryMain {
  def main(args: Array[String]): Unit = {
    val (stockbrokerAsset, minDateOpt, maxDateOpt) = parseArg(args)

    val assetPeriods = ctx.run(
      quote(query[AssetPeriod])
        .dynamic
        .filter(_.asset == lift(stockbrokerAsset.asset))
        .filter(_.stockbroker == lift(stockbrokerAsset.stockbroker))
        .filterOpt(minDateOpt)((ap, minDate) => quote(ap.endDate >= minDate))
        .filterOpt(maxDateOpt)((ap, maxDate) => quote(ap.beginDate <= maxDate))
        .sortBy(_.beginDate)
    )

    val convertedTos = None +: assetPeriods.map(_.convertedTo)

    val printer = new IndentedPrinter
    for {
      (convertedTo, ap) <- convertedTos `zip` assetPeriods
      childNodes = Formatting.forAssetPeriod(ap, convertedTo)
        .collect { case Some(n) => n }
      node = printer.hierarchy.tree(ap.beginDate.toString)(childNodes: _*)
    } {
      printer.hierarchy.print(node)
    }
  }

  private def parseArg(args: Array[String]): (StockbrokerAsset, Option[LocalDate], Option[LocalDate]) = {
    def parseStockbrokerAsset(stringOpt: Option[String]): Try[StockbrokerAsset] =
      stringOpt match {
        case Some(s"$asset:$stockbroker") => Success(StockbrokerAsset(stockbroker, asset))
        case Some(asb) => Failure(new Exception(s"Ativo e/ou corretora inválidos: $asb"))
        case None => Failure(new Exception("Parâmetros faltando"))
      }

    def parseOptionalDate(stringOpt: Option[String]): Try[Option[LocalDate]] =
      Try {
        stringOpt.map(LocalDate.parse)
      } recoverWith { e =>
        Failure(new Exception(s"Data inválida: ${e.getMessage}"))
      }

    def unexpectedArg(stringOpt: Option[String]): Try[Unit] =
      stringOpt match {
        case Some(arg) => Failure(new Exception(s"Parâmetro em excesso: $arg"))
        case None => Success(())
      }

    val argsOpts = args.lift
    val t = for {
      stockbrokerAsset <- parseStockbrokerAsset(argsOpts(0))
      minDateOpt <- parseOptionalDate(argsOpts(1))
      maxDateOpt <- parseOptionalDate(argsOpts(2))
      _ <- unexpectedArg(argsOpts(3))
    } yield (stockbrokerAsset, minDateOpt, maxDateOpt)

    t match {
      case Success(parsedArgs) => parsedArgs
      case Failure(exception) =>
        Console.err.println(exception.getMessage)
        Console.err.println()
        showUsage()
        System.exit(1).asInstanceOf[Nothing]
    }
  }

  private def showUsage(): Unit =
    println("Parâmetros esperados: ATIVO:CORRETORA [DATA-INICIAL [DATA-FINAL]]")
}
