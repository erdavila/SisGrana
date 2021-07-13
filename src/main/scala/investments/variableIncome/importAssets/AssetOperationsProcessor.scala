package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.AssetOperationsProcessor.calculateSwingTradeResultAndUpdatedPosition
import investments.variableIncome.model._
import java.time.LocalDate

class AssetOperationsProcessor(
  stockbroker: String,
  date: LocalDate,
  includeCostToPurchaseAmount: Amount => PurchaseAmountWithCost,
  includeCostToSaleAmount: Amount => SaleAmountWithCost,
) {
  def process(asset: String, operationsAmounts: OperationsAmounts, previousPosition: AmountWithCost): AssetChange = {
    val purchaseAmount = includeCostToPurchaseAmount(operationsAmounts.purchase)
    val saleAmount = includeCostToSaleAmount(operationsAmounts.sale)

    val (dayTradeResult, nonDayTradeAmount) = TradeResult.from(purchaseAmount, saleAmount)
    val (swingTradeResult, updatedPosition) = calculateSwingTradeResultAndUpdatedPosition(previousPosition, nonDayTradeAmount)

    AssetChange
      .withZeroes(asset, stockbroker, date, byEvent = false)
      .withPurchaseAmount(purchaseAmount)
      .withSaleAmount(saleAmount)
      .withDayTradeResult(dayTradeResult)
      .withSwingTradeResult(swingTradeResult)
      .withPosition(updatedPosition)
  }
}

object AssetOperationsProcessor {
  def calculateSwingTradeResultAndUpdatedPosition(
    currentPosition: AmountWithCost,
    nonDayTradeAmount: AmountWithCost,
  ): (TradeResult, AmountWithCost) =
    (currentPosition, nonDayTradeAmount) match {
      case (p1@PurchaseAmountWithCost(_, _, _), p2@PurchaseAmountWithCost(_, _, _)) => (TradeResult.Zero, p1 + p2)
      case (s1@SaleAmountWithCost(_, _, _), s2@SaleAmountWithCost(_, _, _)) => (TradeResult.Zero, s1 + s2)
      case (p@PurchaseAmountWithCost(_, _, _), s@SaleAmountWithCost(_, _, _)) => TradeResult.from(p, s)
      case (s@SaleAmountWithCost(_, _, _), p@PurchaseAmountWithCost(_, _, _)) => TradeResult.from(p, s)
    }
}
