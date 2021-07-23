package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.EventProcessor.EventOutcome
import investments.variableIncome.model.{AmountWithCost, PurchaseAmountWithCost, StockbrokerAsset, TradeResult}

class EventProcessor(event: Event) {
  def process(positionByAsset: Map[StockbrokerAsset, AmountWithCost]): Seq[Map[StockbrokerAsset, EventOutcome]] = {
    val assetsPositionsByStockbroker = positionByAsset
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, position) => (stockbrokerAsset.asset, position) }

    assetsPositionsByStockbroker
      .map { case (stockbroker, assetsPositions) =>
        processEventOnStockBrokerAssets(assetsPositions.toMap)
          .map { case (asset, positionAndTradeResult) => StockbrokerAsset(stockbroker, asset) -> positionAndTradeResult }
      }
      .toSeq
  }

  private def processEventOnStockBrokerAssets(positionByAsset: Map[String, AmountWithCost]): Map[String, EventOutcome] =
    calculateOutcome(positionByAsset)
      .getOrElse(Map.empty)

  private[importAssets] def calculateOutcome(
    positionByAsset: Map[String, AmountWithCost],
  ): Option[Map[String, EventOutcome]] =
    positionByAsset.get(event.from.asset).map { matchingAssetPosition =>
      val times = matchingAssetPosition.quantity / event.from.quantity
      val removedQuantity = times * event.from.quantity
      val newQuantity = matchingAssetPosition.quantity - removedQuantity
      val newMatchingAssetPosition = matchingAssetPosition.withQuantity(newQuantity)

      val eventOutcomeByAsset =
        (positionByAsset + (event.from.asset -> newMatchingAssetPosition))
          .view.mapValues(EventOutcome(TradeResult.Zero, _))
          .toMap

      event.tos.foldLeft(eventOutcomeByAsset) { (eventOutcomeByAsset, to) =>
        eventOutcomeByAsset.updatedWith(to.asset) { eventOutcomeOpt =>
          val eventOutcome = eventOutcomeOpt.getOrElse(EventOutcome(TradeResult.Zero, PurchaseAmountWithCost.Zero))
          val (averagePrice, averageCost) = to.averagePriceDefinition(
            matchingAssetPosition.averagePrice,
            matchingAssetPosition.averageCost,
          )

          val quantity = times * to.quantity
          val amount = AmountWithCost.fromSignedQuantityAndAverages(quantity, averagePrice, averageCost)

          val (newUpdatedPosition, eventSwingTradeResult) = AmountWithCost.combine(eventOutcome.position, amount)
          Some(EventOutcome(eventOutcome.tradeResult + eventSwingTradeResult, newUpdatedPosition))
        }
      }
    }
}

object EventProcessor {
  case class EventOutcome(tradeResult: TradeResult, position: AmountWithCost)
}
