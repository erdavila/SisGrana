package sisgrana
package investments.assetsAtDate

import investments.AssetType
import investments.model._
import investments.model.ctx._
import java.time.LocalDate
import utils.{BrNumber, IndentedPrinter}

object AssetsAtDateMain extends LocalDateSupport {
  private lazy val typeResolver = AssetType.Resolver.instance

  def main(args: Array[String]): Unit = {
    val date = args.headOption.fold(LocalDate.now())(LocalDate.parse)
    showAssetsAtDate(date)
  }

  def showAssetsAtDate(date: LocalDate, printer: IndentedPrinter = new IndentedPrinter): Unit = {
    val result = ctx.run(
      query[AssetPeriod]
        .filter(_.beginDate <= lift(date))
        .filter(lift(date) < _.endDate)
        .filter(_.resultingPositionQuantity != 0)
    )

    val text =
      if (date `isAfter` LocalDate.now()) "Ativos até o momento"
      else s"Ativos em $date"

    printer.context(text) {
      for ((stockbroker, assetPeriods) <- result.groupBy(_.stockbroker).toIndexedSeq.sortBy(_._1)) {
        printer.context(stockbroker) {
          for (ap <- assetPeriods.sortBy(_.asset)) {
            val position = ap.resultingPosition
            val tag = typeTag(ap.asset).fold("")(text => s" [$text]")
            printer.println(s"${ap.asset}: ${position.signedQuantity} x ${BrNumber.formatMoney(position.averagePriceWithCost)} = ${BrNumber.formatMoney(position.signedNetValue)}$tag")
          }
        }
      }
    }
  }

  private def typeTag(asset: String): Option[String] =
    typeResolver.resolve(asset) match {
      case AssetType.Stock => None
      case AssetType.ETF => Some("ETF")
      case AssetType.EtfRendaFixa => Some("ETF de Renda Fixa")
      case AssetType.FII => Some("FII")
      case AssetType.Option => Some("Opção")
    }
}
