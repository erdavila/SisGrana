package sisgrana
package investments.variableIncome.assetsAtDate

import investments.irpf.Main.TypesFileName
import investments.utils.BrNumber
import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import investments.variableIncome.{AssetType, DataPath}
import java.io.File
import java.time.LocalDate
import utils.IndentedPrinter

object Main extends LocalDateSupport {
  private lazy val typeResolver = AssetType.Resolver.fromFile(new File(DataPath, TypesFileName))

  def main(args: Array[String]): Unit = {
    val date = args.headOption.fold(LocalDate.now())(LocalDate.parse)

    val result = ctx.run(
      AssetChange.latestAssetChangesAtDateQuery(date)
        .filter(_.resultingPositionQuantity != 0)
    )


    val printer = new IndentedPrinter
    for ((stockbroker, assetChanges) <- result.groupBy(_.stockbroker).toIndexedSeq.sortBy(_._1)) {
      printer.context(stockbroker) {
        for (ac <- assetChanges.sortBy(_.asset)) {
          val position = ac.resultingPosition
          val tag = typeTag(ac.asset).fold("")(text => s" [$text]")
          printer.println(s"${ac.asset} ${position.signedQuantity} x ${BrNumber.formatMoney(position.averagePriceWithCost)} = ${BrNumber.formatMoney(position.signedTotalValueWithCost)}$tag")
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
