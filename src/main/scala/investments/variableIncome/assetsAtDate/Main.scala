package sisgrana
package investments.variableIncome.assetsAtDate

import investments.utils.BrNumber
import investments.variableIncome.model.ctx._
import investments.variableIncome.model._
import java.time.LocalDate
import utils.IndentedPrinter

object Main extends LocalDateSupport {

  def main(args: Array[String]): Unit = {
    val date = args.headOption.fold(LocalDate.now())(LocalDate.parse)

    val result = ctx.run(
      AssetChange.latestAssetChangesAtDateQuery(date)
        .filter(_.positionQuantity != 0)
    )

    val printer = new IndentedPrinter
    for ((stockbroker, assetChanges) <- result.groupBy(_.stockbroker).toIndexedSeq.sortBy(_._1)) {
      printer.context(stockbroker) {
        for (ac <- assetChanges.sortBy(_.asset)) {
          val position = ac.position
          printer.println(s"${ac.asset} ${position.signedQuantity} x ${BrNumber.formatMoney(position.averagePriceWithCost)} = ${BrNumber.formatMoney(position.signedTotalValueWithCost)}")
        }
      }
    }
  }
}
