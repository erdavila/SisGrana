package sisgrana
package investments.variableIncome.assetsAtDate

import investments.irpf.IndentedPrinter
import investments.utils.BrNumber
import investments.variableIncome.model.ctx._
import investments.variableIncome.model.{AssetChange, LocalDateSupport, ctx}
import java.time.LocalDate

object Main extends LocalDateSupport {

  def main(args: Array[String]): Unit = {
    val date = args.headOption.fold(LocalDate.now())(LocalDate.parse)

    val result = ctx.run {
      val latestChanges = query[AssetChange]
        .filter(ac => ac.date <= lift(date))
        .groupBy(ac => (ac.asset, ac.stockbroker))
        .map { case ((asset, stockbroker), assetChanges) => (asset, stockbroker, assetChanges.map(_.date).max) }

      for {
        ac <- query[AssetChange]
          .filter(_.resultingQuantity > 0)
          .sortBy(ac => (ac.stockbroker, ac.asset))
        (asset, stockbroker, dateOpt) <- latestChanges
        if ac.asset == asset && ac.stockbroker == stockbroker && dateOpt.contains(ac.date)
      } yield ac
    }

    val printer = new IndentedPrinter
    for ((stockbroker, assetChanges) <- result.groupBy(_.stockbroker).toIndexedSeq.sortBy(_._1)) {
      printer.context(stockbroker) {
        for (ac <- assetChanges.sortBy(_.asset)) {
          val amountWithCost = ac.resultingAmountWithCost
          printer.println(s"${ac.asset} ${amountWithCost.quantity} x ${BrNumber.formatMoney(amountWithCost.averagePriceWithCost)} = ${BrNumber.formatMoney(amountWithCost.totalValueWithCost)}")
        }
      }
    }
  }
}
