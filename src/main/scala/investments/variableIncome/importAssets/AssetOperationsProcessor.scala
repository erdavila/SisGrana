package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.AssetOperationsProcessor.OperationsOutcome
import investments.variableIncome.model._

class AssetOperationsProcessor(
  includeCostToPurchaseAmount: Amount => PurchaseAmountWithCost,
  includeCostToSaleAmount: Amount => SaleAmountWithCost,
) {
  def process(operationsAmounts: OperationsAmounts, previousPosition: AmountWithCost): OperationsOutcome = {
    val purchaseAmount = includeCostToPurchaseAmount(operationsAmounts.purchase)
    val saleAmount = includeCostToSaleAmount(operationsAmounts.sale)

    val (dayTradeResult, nonDayTradeAmount) = TradeResult.from(purchaseAmount, saleAmount)
    val (resultingPosition, swingTradeResult) = AmountWithCost.combine(previousPosition, nonDayTradeAmount)

    OperationsOutcome(
      purchaseAmount,
      saleAmount,
      dayTradeResult,
      nonDayTradeAmount,
      swingTradeResult,
      resultingPosition,
    )
  }
}

object AssetOperationsProcessor {
  case class OperationsOutcome(
    purchaseAmount: PurchaseAmountWithCost,
    saleAmount: SaleAmountWithCost,
    dayTradeResult: TradeResult,
    nonDayTradeAmount: AmountWithCost,
    swingTradeResult: TradeResult,
    position: AmountWithCost,
  )
}
