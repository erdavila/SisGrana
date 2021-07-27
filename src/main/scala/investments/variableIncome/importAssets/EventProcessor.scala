package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.EventProcessor.{mergeEffectsByAsset, processBonus, processConversion}
import investments.variableIncome.model.{AmountWithCost, EventEffect, PurchaseAmountWithCost, StockbrokerAsset}

class EventProcessor(event: Event) {
  def process(positionByAsset: Map[StockbrokerAsset, AmountWithCost]): Map[StockbrokerAsset, EventEffect] = {
    val assetsPositionsByStockbroker = positionByAsset
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, position) => (stockbrokerAsset.asset, position) }

    assetsPositionsByStockbroker
      .map { case (stockbroker, assetsPositions) =>
        processEventOnStockbrokerAssets(assetsPositions.toMap)
          .map { case (asset, eventEffect) => StockbrokerAsset(stockbroker, asset) -> eventEffect }
      }
      .reduceOption(mergeEffectsByAsset)
      .getOrElse(Map.empty)
  }

  private def processEventOnStockbrokerAssets(positionByAsset: Map[String, AmountWithCost]): Map[String, EventEffect] =
    event match {
      case c@Event.Conversion(_, _, _, _) => processConversion(c, positionByAsset)
      case b@Event.Bonus(_, _, _, _, _) => processBonus(b, positionByAsset)
    }
}

object EventProcessor {
  def mergeEffectsByAsset(
    effects1: Map[StockbrokerAsset, EventEffect],
    effects2: Map[StockbrokerAsset, EventEffect],
  ): Map[StockbrokerAsset, EventEffect] =
    effects1.foldLeft(effects2) { case (effects1, (stockbrokerAsset2, effect2)) =>
      effects1.updatedWith(stockbrokerAsset2) {
        case Some(effect1) => Some(mergeEffects(stockbrokerAsset2)(effect1, effect2))
        case None => Some(effect2)
      }
    }

  private def mergeEffects(stockbrokerAsset: StockbrokerAsset)(effect1: EventEffect, effect2: EventEffect): EventEffect =
    (effect1, effect2) match {
      case (EventEffect.AddToPosition(p1, s1), EventEffect.AddToPosition(p2, s2)) => EventEffect.AddToPosition(p1 + p2, s1 + s2)
      case _ => throw new Exception(s"Efeitos dos tipos ${effect1.getClass} e ${effect2.getClass} não podem ser mesclados ($stockbrokerAsset)")
    }

  private[importAssets] def processConversion(conversion: Event.Conversion, positionByAsset: Map[String, AmountWithCost]): Map[String, EventEffect] =
    positionByAsset.get(conversion.fromAsset) match {
      case Some(position) =>
        val newQuantity = (position.signedQuantity * conversion.toQuantity / conversion.fromQuantity).toInt
        val amount = AmountWithCost.fromSignedQuantityAndAverages(
          signedQuantity = newQuantity,
          averagePrice = position.averagePrice * conversion.fromQuantity / conversion.toQuantity,
          averageCost = position.averageCost * conversion.fromQuantity / conversion.toQuantity,
        )
        if (conversion.fromAsset == conversion.toAsset) {
          Map(
            conversion.toAsset -> EventEffect.SetPosition(amount),
          )
        } else {
          Map(
            conversion.fromAsset -> EventEffect.SetPosition(PurchaseAmountWithCost.Zero),
            conversion.toAsset -> EventEffect.AddToPosition(amount),
          )
        }
      case None => Map.empty
    }

  private[importAssets] def processBonus(bonus: Event.Bonus, positionByAsset: Map[String, AmountWithCost]): Map[String, EventEffect] =
    positionByAsset.get(bonus.fromAsset) match {
      case Some(position) if position.signedQuantity > 0 =>
        val newQuantity = (position.quantity * bonus.toQuantity / bonus.fromQuantity).toInt
        val amount = AmountWithCost.fromSignedQuantityAndAverages(
          newQuantity,
          averagePrice = bonus.toPrice,
          averageCost = 0.0,
        )
        Map(
          bonus.toAsset -> EventEffect.AddToPosition(amount)
        )
      case _ => Map.empty
    }
}
