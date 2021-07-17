package sisgrana
package investments.variableIncome.model

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class TradeResultTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("TradeResult.from()") {
    val purchase = AmountWithCostTest.PurchaseDSL
    val sale = AmountWithCostTest.SaleDSL
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
        purchase.averages(10, 2.00, 0.01),
        sale.Zero,
        expectedDayTradeResult.Zero,
        expectedRemainingAmount.purchase.averages(10, 2.00, 0.01),
      ),
      (
        purchase.Zero,
        sale.averages(10, 2.00, 0.01),
        expectedDayTradeResult.Zero,
        expectedRemainingAmount.sale.averages(10, 2.00, 0.01),
      ),
      (
        purchase.averages(10, 4.00, 0.02),
        sale.averages(7, 3.00, 0.02),
        expectedDayTradeResult.averages(7, 4.00, 0.02, 3.00, 0.02),
        expectedRemainingAmount.purchase.averages(3, 4.00, 0.02),
      ),
      (
        purchase.averages(10, 4.00, 0.05),
        sale.averages(12, 5.00, 0.03),
        expectedDayTradeResult.averages(10, 4.00, 0.05, 5.00, 0.03),
        expectedRemainingAmount.sale.averages(2, 5.00, 0.03),
      ),
      (
        purchase.averages(10, 4.00, 0.05),
        sale.averages(10, 5.00, 0.03),
        expectedDayTradeResult.averages(10, 4.00, 0.05, 5.00, 0.03),
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
    def averages: (Int, Double, Double, Double, Double) => TradeResult = TradeResult.fromAverages
    def totals: (Int, Double, Double, Double, Double) => TradeResult = TradeResult.fromTotals
  }
}
