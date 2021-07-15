package sisgrana
package investments.variableIncome.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TradeResultTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("TradeResult.from()") {
    object purchase {
      val Zero: PurchaseAmountWithCost = PurchaseAmountWithCost.Zero
      def apply(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost = PurchaseAmountWithCost.fromTotals(quantity, totalValue, totalCost)
    }
    object sale {
      val Zero: SaleAmountWithCost = SaleAmountWithCost.Zero
      def apply(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost = SaleAmountWithCost.fromTotals(quantity, totalValue, totalCost)
    }
    val expectedDayTradeResult = TradeResultTest.DSL
    val expectedRemainingAmount = AmountWithCostTest.DSL

    val cases = Table(
      (
        "purchase",
        "sale",
        "expected dayTradeResult",
        "expected remainingAmount",
      ),
      (
        purchase.Zero,
        sale.Zero,
        expectedDayTradeResult.Zero,
        expectedRemainingAmount.Zero,
      ),
      (
        purchase(10, 20.00, 0.10),
        sale.Zero,
        expectedDayTradeResult.Zero,
        expectedRemainingAmount.purchase(10, 20.00, 0.10),
      ),
      (
        purchase.Zero,
        sale(10, 20.00, 0.10),
        expectedDayTradeResult.Zero,
        expectedRemainingAmount.sale(10, 20.00, 0.10),
      ),
      (
        purchase(10, 40.00, 0.20),
        sale(7, 21.00, 0.14),
        expectedDayTradeResult(7, -7.00, 0.28),
        expectedRemainingAmount.purchase(3, 12.00, 0.06),
      ),
      (
        purchase(10, 40.00, 0.20),
        sale(12, 60.00, 0.36),
        expectedDayTradeResult(10, 10.00, 0.50),
        expectedRemainingAmount.sale(2, 10.00, 0.06),
      ),
      (
        purchase(10, 40.00, 0.20),
        sale(10, 50.00, 0.30),
        expectedDayTradeResult(10, 10.00, 0.50),
        expectedRemainingAmount.Zero,
      ),
    )

    forAll(cases) { case (purchase, sale, expectedDayTradeResult, expectedRemainingAmount) =>
      val (dayTradeResult, remainingAmount) = TradeResult.from(purchase, sale)
      dayTradeResult should equal (expectedDayTradeResult)
      remainingAmount should equal (expectedRemainingAmount)
    }
  }
}

object TradeResultTest {
  object DSL {
    val Zero: TradeResult = TradeResult.Zero
    def apply(quantity: Int, grossValue: Double, totalCost: Double): TradeResult = TradeResult(quantity, grossValue, totalCost)
  }
}
