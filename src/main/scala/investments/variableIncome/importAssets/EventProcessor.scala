package sisgrana
package investments.variableIncome.importAssets

import investments.irpf.StockbrokerAsset
import investments.variableIncome.model._

class EventProcessor(event: Event) {
  def process(positionByAsset: Map[StockbrokerAsset, AmountWithCost]): Seq[Map[StockbrokerAsset, (AmountWithCost, TradeResult)]] = {
    val assetsPositionsByStockbroker = positionByAsset
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, position) => (stockbrokerAsset.asset, position) }

    assetsPositionsByStockbroker
      .map { case (stockbroker, assetsPositions) =>
        processEventOnStockBrokerAssets(assetsPositions.toMap)
          .map { case (asset, positionAndTradeResult) => StockbrokerAsset(stockbroker, asset) -> positionAndTradeResult }
      }
      .toSeq
  }

  private def processEventOnStockBrokerAssets(positionByAsset: Map[String, AmountWithCost]): Map[String, (AmountWithCost, TradeResult)] =
    calculateUpdatedPositionsAndSwingTradeResult(positionByAsset)
      .getOrElse(Map.empty)

  private[importAssets] def calculateUpdatedPositionsAndSwingTradeResult(
    positionByAsset: Map[String, AmountWithCost],
  ): Option[Map[String, (AmountWithCost, TradeResult)]] =
    positionByAsset.get(event.from.asset).map { matchingAssetPosition =>
      val times = matchingAssetPosition.quantity / event.from.quantity
      val removedQuantity = times * event.from.quantity
      val newQuantity = matchingAssetPosition.quantity - removedQuantity
      val newMatchingAssetPosition = matchingAssetPosition.withQuantity(newQuantity)

      val positionAndSwingTradeResultByAsset =
        (positionByAsset + (event.from.asset -> newMatchingAssetPosition))
          .view.mapValues((_, TradeResult.Zero))
          .toMap

      event.tos.foldLeft(positionAndSwingTradeResultByAsset) { (positionAndSwingTradeResultByAsset, to) =>
        positionAndSwingTradeResultByAsset.updatedWith(to.asset) { positionAndSwingTradeResultOpt =>
          val (position, swingTradeResult) = positionAndSwingTradeResultOpt.getOrElse((PurchaseAmountWithCost.Zero, TradeResult.Zero))
          val (averagePrice, averageCost) = to.averagePriceDefinition(
            matchingAssetPosition.averagePrice,
            matchingAssetPosition.averageCost,
          )

          val quantity = times * to.quantity
          val amount = AmountWithCost.fromSignedQuantityAndAverages(quantity, averagePrice, averageCost)

          val (eventSwingTradeResult, newUpdatedPosition) = AssetOperationsProcessor.calculateSwingTradeResultAndUpdatedPosition(position, amount)
          Some((newUpdatedPosition, swingTradeResult + eventSwingTradeResult))
        }
      }
    }
}
