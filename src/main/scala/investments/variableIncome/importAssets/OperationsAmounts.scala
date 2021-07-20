package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{Amount, PurchaseAmountWithCost, SaleAmountWithCost}

case class OperationsAmounts(purchase: Amount, sale: Amount)

object OperationsAmounts {
  case class WithCost(purchase: PurchaseAmountWithCost, sale: SaleAmountWithCost)
}
