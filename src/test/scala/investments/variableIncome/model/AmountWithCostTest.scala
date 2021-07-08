package sisgrana
package investments.variableIncome.model

import org.scalatest.funsuite.AnyFunSuite

class AmountWithCostTest extends AnyFunSuite {

}

object AmountWithCostTest {
  object DSL {
    val Zero: PurchaseAmountWithCost = PurchaseAmountWithCost.Zero
    def purchase(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost = PurchaseAmountWithCost(quantity, totalValue, totalCost)
    def sale(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost = SaleAmountWithCost(quantity, totalValue, totalCost)
  }
}
