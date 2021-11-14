package sisgrana
package investments.multiImport.eventsAndBrokerageNotes

import investments.model.{Amount, PurchaseAmount, StockbrokerAsset}
import investments.multiImport.eventsAndBrokerageNotes.EventProcessor.{mergeOutcomeByAsset, processBonus, processConversion}

class EventProcessor(event: Event) {
  def process(positionByAsset: Map[StockbrokerAsset, Amount]): Map[StockbrokerAsset, EventOutcome] = {
    val assetsPositionsByStockbroker = positionByAsset
      .groupMap(_._1.stockbroker) { case (stockbrokerAsset, position) => (stockbrokerAsset.asset, position) }

    assetsPositionsByStockbroker
      .map { case (stockbroker, assetsPositions) =>
        processEventOnStockbrokerAssets(assetsPositions.toMap)
          .map { case (asset, eventOutcome) => StockbrokerAsset(stockbroker, asset) -> eventOutcome }
      }
      .reduceOption(mergeOutcomeByAsset)
      .getOrElse(Map.empty)
  }

  private def processEventOnStockbrokerAssets(positionByAsset: Map[String, Amount]): Map[String, EventOutcome] =
    event match {
      case c@Event.Conversion(_, _, _, _) => processConversion(c, positionByAsset)
      case b@Event.Bonus(_, _, _, _, _) => processBonus(b, positionByAsset)
    }
}

object EventProcessor {
  def mergeOutcomeByAsset(
    outcomes1: Map[StockbrokerAsset, EventOutcome],
    outcomes2: Map[StockbrokerAsset, EventOutcome],
  ): Map[StockbrokerAsset, EventOutcome] =
    outcomes1.foldLeft(outcomes2) { case (outcomes1, (stockbrokerAsset2, outcome2)) =>
      outcomes1.updatedWith(stockbrokerAsset2) {
        case Some(outcome1) => Some(mergeOutcomes(stockbrokerAsset2)(outcome1, outcome2))
        case None => Some(outcome2)
      }
    }

  private def mergeOutcomes(stockbrokerAsset: StockbrokerAsset)(outcome1: EventOutcome, outcome2: EventOutcome): EventOutcome =
    (outcome1, outcome2) match {
      case (EventOutcome.AddToPosition(p1, s1), EventOutcome.AddToPosition(p2, s2)) => EventOutcome.AddToPosition(p1 + p2, s1 + s2)
      case _ => throw new Exception(s"Efeitos dos tipos ${outcome1.getClass} e ${outcome2.getClass} não podem ser mesclados ($stockbrokerAsset)")
    }

  private[eventsAndBrokerageNotes] def processConversion(conversion: Event.Conversion, positionByAsset: Map[String, Amount]): Map[String, EventOutcome] =
    positionByAsset.get(conversion.fromAsset) match {
      case Some(position) =>
        val newQuantity = position.signedQuantity * conversion.toQuantity / conversion.fromQuantity
        val amount = Amount.fromSignedQuantityAndAverages(
          signedQuantity = newQuantity.toInt,
          averagePrice = position.averagePrice * conversion.fromQuantity / conversion.toQuantity,
          averageCost = position.averageCost * conversion.fromQuantity / conversion.toQuantity,
        )

        def setPositionEntry(amount: Amount) =
          conversion.fromAsset -> EventOutcome.SetPosition(amount, conversion.toAsset, newQuantity)

        if (conversion.fromAsset == conversion.toAsset) {
          Map(
            setPositionEntry(amount),
          )
        } else {
          Map(
            setPositionEntry(PurchaseAmount.Zero),
            conversion.toAsset -> EventOutcome.AddToPosition(amount),
          )
        }
      case None => Map.empty
    }

  private[eventsAndBrokerageNotes] def processBonus(bonus: Event.Bonus, positionByAsset: Map[String, Amount]): Map[String, EventOutcome] =
    positionByAsset.get(bonus.fromAsset) match {
      case Some(position) if position.signedQuantity > 0 =>
        val newQuantity = (position.quantity * bonus.toQuantity / bonus.fromQuantity).toInt
        val amount = Amount.fromSignedQuantityAndAverages(
          newQuantity,
          averagePrice = bonus.toPrice,
          averageCost = 0.0,
        )
        Map(
          bonus.toAsset -> EventOutcome.AddToPosition(amount)
        )
      case _ => Map.empty
    }
}
