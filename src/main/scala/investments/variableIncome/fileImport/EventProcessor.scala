package sisgrana
package investments.variableIncome.fileImport

import investments.irpf.StockbrokerAsset
import investments.variableIncome.model.ctx._
import investments.variableIncome.model.{AssetChange, CustomEncoding, ctx}
import java.time.LocalDate

class EventProcessor(event: Event, eventDate: LocalDate) extends CustomEncoding {
  def process(): Unit = {
    val involvedAssets = event.tos.map(_.asset).toSet + event.from.asset
    val assetsAmountsWithCostByStockBroker = getInvolvedAssetsAmountsWithCost(involvedAssets)
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, amountWithCost) => (stockbrokerAsset.asset, amountWithCost) }

    for ((stockbroker, assetsAmountsWithCost) <- assetsAmountsWithCostByStockBroker) {
      processEventOnStockBrokerAssets(stockbroker, assetsAmountsWithCost.toMap)
    }
  }

  private def getInvolvedAssetsAmountsWithCost(assets: Set[String]): Map[StockbrokerAsset, AmountWithCost] = {
    val result = ctx.run {
      val latestAssetChange = query[AssetChange]
        .filter(ac => liftQuery(assets).contains(ac.asset))
        .groupBy(ac => (ac.asset, ac.stockbroker))
        .map { case ((asset, stockbroker), assetChanges) =>
          (asset, stockbroker, assetChanges.map(_.date).max)
        }

      for {
        (asset, stockbroker, dateOpt) <- latestAssetChange
        ac <- query[AssetChange]
        if ac.asset == asset && ac.stockbroker == stockbroker && dateOpt.contains(ac.date)
      } yield ac
    }

    result
      .map { assetChange =>
        if (!assetChange.date.isBefore(eventDate)) {
          throw new Exception(s"Encontrado registro com data igual ou posterior ao evento: $assetChange")
        }

        StockbrokerAsset(assetChange.stockbroker, assetChange.asset) -> assetChange.resultingAmountWithCost
      }
      .toMap
  }

  private def processEventOnStockBrokerAssets(stockbroker: String, amountsWithCostByAsset: Map[String, AmountWithCost]): Unit =
    for (matchingAssetAmountWithCost <- amountsWithCostByAsset.get(event.from.asset)) {
      val times = matchingAssetAmountWithCost.quantity / event.from.quantity
      val removedQuantity = times * event.from.quantity
      val newMatchingAssetAmountWithCost = matchingAssetAmountWithCost.modifyQuantity(_ - removedQuantity)

      val newAmountsWithCostByAsset = amountsWithCostByAsset + (event.from.asset -> newMatchingAssetAmountWithCost)

      val updatedAmountsWithCostByAsset = event.tos.foldLeft(newAmountsWithCostByAsset) { (amountsWithCostByAsset, to) =>
        amountsWithCostByAsset.updatedWith(to.asset) { amountWithCostOpt =>
          val amountWithCost = amountWithCostOpt.getOrElse(AmountWithCost.Zero)
          val (averagePrice, averageCost) = to.averagePriceDefinition(
            matchingAssetAmountWithCost.averagePrice,
            matchingAssetAmountWithCost.averageCost,
          )

          val newAmountWithCost = AmountWithCost.fromAveragePriceAndCost(times * to.quantity, averagePrice, averageCost)
          val updatedAmountWithCost = amountWithCost + newAmountWithCost
          Some(updatedAmountWithCost)
        }
      }

      for ((asset, amountWithCost) <- updatedAmountsWithCostByAsset) {
        save(asset, amountWithCost, stockbroker)
      }
    }

  private def save(asset: String, amountWithCost: AmountWithCost, stockbroker: String): Unit = {
    val assetChange = AssetChange.fromAmountsWithCost(asset, stockbroker, eventDate, byEvent = true)(
      AmountWithCost.Zero,
      AmountWithCost.Zero,
      amountWithCost,
    )

    ctx.run {
      query[AssetChange]
        .insert(lift(assetChange))
    }
  }
}
