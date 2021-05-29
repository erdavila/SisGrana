package sisgrana
package investments.variableIncome.model

import investments.variableIncome.fileImport.Operation
import java.time.LocalDate

case class AssetChange(
  asset: String,
  stockbroker: String,
  date: LocalDate,

  byEvent: Boolean,

  purchaseQuantity: Int,
  purchaseTotalValue: Double,
  purchaseCostTotal: Double,

  saleQuantity: Int,
  saleTotalValue: Double,
  saleCostTotal: Double,

  resultingQuantity: Int,
  resultingTotalValue: Double,
  resultingCostTotalValue: Double,
) {
  assert(purchaseQuantity >= 0)
  assert(purchaseTotalValue >= 0.0)
  assert(purchaseCostTotal >= 0.0)
  assert(saleQuantity >= 0)
  assert(saleTotalValue >= 0.0)
  assert(saleCostTotal >= 0.0)
  assert(resultingQuantity >= 0)
  assert(resultingTotalValue >= 0.0)
  assert(resultingCostTotalValue >= 0.0)

  lazy val purchaseAmountWithCost: AmountWithCost =
    AmountWithCost(purchaseQuantity, purchaseTotalValue, purchaseCostTotal, Operation.Purchase)

  lazy val saleAmountWithCost: AmountWithCost =
    AmountWithCost(saleQuantity, saleTotalValue, saleCostTotal, Operation.Sale)

  lazy val resultingAmountWithCost: AmountWithCost =
    AmountWithCost(resultingQuantity, resultingTotalValue, resultingCostTotalValue, Operation.Purchase)
}

object AssetChange {
  def fromAmountsWithCost(asset: String, stockbroker: String, date: LocalDate, byEvent: Boolean = false)(
    purchaseAmountWithCost: AmountWithCost,
    saleAmountWithCost: AmountWithCost,
    resultingAmountWithCost: AmountWithCost,
  ): AssetChange =
    AssetChange(
      asset,
      stockbroker,
      date,
      byEvent,
      purchaseAmountWithCost.quantity,
      purchaseAmountWithCost.totalValue,
      purchaseAmountWithCost.totalCost,
      saleAmountWithCost.quantity,
      saleAmountWithCost.totalValue,
      saleAmountWithCost.totalCost,
      resultingAmountWithCost.quantity,
      resultingAmountWithCost.totalValue,
      resultingAmountWithCost.totalCost,
    )
}
