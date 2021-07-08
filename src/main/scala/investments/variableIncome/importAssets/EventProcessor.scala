package sisgrana
package investments.variableIncome.importAssets

import investments.irpf.StockbrokerAsset
import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import java.time.LocalDate

class EventProcessor(event: Event, eventDate: LocalDate) extends LocalDateSupport {
  def process(): Unit = {
    val involvedAssets = event.tos.map(_.asset).toSet + event.from.asset
    val assetsPositionsAndSwingTradeResultByStockbroker = getInvolvedAssetsPositionsAndSwingTradeResults(involvedAssets)
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, positionAndSwingTradeResult) => (stockbrokerAsset.asset, positionAndSwingTradeResult) }

    for ((stockbroker, assetsPositionsAndSwingTradeResult) <- assetsPositionsAndSwingTradeResultByStockbroker) {
      processEventOnStockBrokerAssets(stockbroker, assetsPositionsAndSwingTradeResult.toMap)
    }
  }

  private def getInvolvedAssetsPositionsAndSwingTradeResults(assets: Set[String]): Map[StockbrokerAsset, (AmountWithCost, TradeResult)] = {
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

        assetChange.stockbrokerAsset -> ((assetChange.position, assetChange.swingTradeResult))
      }
      .toMap
  }

  private def processEventOnStockBrokerAssets(stockbroker: String, positionAndSwingTradeResultByAsset: Map[String, (AmountWithCost, TradeResult)]): Unit =
    for {
      updatedPositionAndSwingTradeResultByAsset <- calculateUpdatedPositions(positionAndSwingTradeResultByAsset)
      (asset, (position, swingTradeResult)) <- updatedPositionAndSwingTradeResultByAsset
    } {
      save(asset, position, swingTradeResult, stockbroker)
    }

  private[importAssets] def calculateUpdatedPositions(
    positionAndSwingTradeResultByAsset: Map[String, (AmountWithCost, TradeResult)],
  ): Option[Map[String, (AmountWithCost, TradeResult)]] =
    positionAndSwingTradeResultByAsset.get(event.from.asset).map { case (matchingAssetPosition, matchingAssetSwingTradeResult) =>
      val times = matchingAssetPosition.quantity / event.from.quantity
      val removedQuantity = times * event.from.quantity
      val newQuantity = matchingAssetPosition.quantity - removedQuantity
      val newMatchingAssetPosition = matchingAssetPosition.withQuantity(newQuantity)

      val newPositionAndSwingTradeResultByAsset = positionAndSwingTradeResultByAsset +
        (event.from.asset -> ((newMatchingAssetPosition, matchingAssetSwingTradeResult)))

      event.tos.foldLeft(newPositionAndSwingTradeResultByAsset) { (positionAndSwingTradeResultByAsset, to) =>
        positionAndSwingTradeResultByAsset.updatedWith(to.asset) { positionAndSwingTradeResultOpt =>
          val (position, swingTradeResult) = positionAndSwingTradeResultOpt.getOrElse((PurchaseAmountWithCost.Zero, TradeResult.Zero))
          val (averagePrice, averageCost) = to.averagePriceDefinition(
            matchingAssetPosition.averagePrice,
            matchingAssetPosition.averageCost,
          )

          val quantity = times * to.quantity
          val amount =
            if (quantity >= 0) {
              PurchaseAmountWithCost(quantity, quantity * averagePrice, quantity * averageCost)
            } else {
              SaleAmountWithCost(-quantity, -quantity * averagePrice, -quantity * averageCost)
            }

          val (eventSwingTradeResult, newUpdatedPosition) = AssetOperationsProcessor.calculateSwingTradeResultAndUpdatedPositionAmount(position, amount)
          Some((newUpdatedPosition, swingTradeResult + eventSwingTradeResult))
        }
      }
    }

  private def save(asset: String, position: AmountWithCost, swingTradeResult: TradeResult, stockbroker: String): Unit = {
    val assetChange = AssetChange.withZeroes(asset, stockbroker, eventDate, byEvent = true)
      .withPosition(position)
      .withSwingTradeResult(swingTradeResult)

    ctx.run {
      query[AssetChange]
        .insert(lift(assetChange))
    }
  }
}
