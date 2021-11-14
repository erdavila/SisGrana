package sisgrana
package investments.model

class AmountTest extends TestBase {
  test("Amount.combine()") {
    val currentPositionAmount = AmountTest.DSL
    val nonDayTradeAmount = AmountTest.DSL
    val expectedSwingTradeResult = TradeResultTest.DSL
    val expectedUpdatedPositionAmount = AmountTest.DSL

    val cases = Table(
      (
        "currentPositionAmount",
        "nonDayTradeAmount",
        "expected swingTradeResult",
        "expected updatedPositionAmount",
      ),
      (
        currentPositionAmount.Zero,
        nonDayTradeAmount.Zero,
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.Zero,
      ),
      (
        currentPositionAmount.Zero,
        nonDayTradeAmount.purchase.totals(10, 30.00, 0.20),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase.totals(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.Zero,
        nonDayTradeAmount.sale.totals(10, 30.00, 0.20),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale.totals(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.purchase.totals(10, 30.00, 0.20),
        nonDayTradeAmount.Zero,
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase.totals(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.purchase.totals(10, 30.00, 0.20),
        nonDayTradeAmount.purchase.totals(3, 12.00, 0.12),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase.totals(13, 42.00, 0.32),
      ),
      (
        currentPositionAmount.purchase.averages(10, 3.00, 0.02),
        nonDayTradeAmount.sale.averages(3, 4.00, 0.04),
        expectedSwingTradeResult.averages(3, 3.00, 0.02, 4.00, 0.04),
        expectedUpdatedPositionAmount.purchase.averages(7, 3.00, 0.02),
      ),
      (
        currentPositionAmount.purchase.totals(10, 30.00, 0.20),
        nonDayTradeAmount.sale.totals(10, 20.00, 0.12),
        expectedSwingTradeResult.totals(10, 30.00, 0.20, 20.00, 0.12),
        expectedUpdatedPositionAmount.Zero,
      ),
      (
        currentPositionAmount.purchase.averages(10, 3.00, 0.02),
        nonDayTradeAmount.sale.averages(12, 4.00, 0.03),
        expectedSwingTradeResult.averages(10, 3.00, 0.02, 4.00, 0.03),
        expectedUpdatedPositionAmount.sale.averages(2, 4.00, 0.03),
      ),
      (
        currentPositionAmount.sale.totals(10, 30.00, 0.20),
        nonDayTradeAmount.Zero,
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale.totals(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.sale.totals(10, 30.00, 0.20),
        nonDayTradeAmount.sale.totals(3, 12.00, 0.12),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale.totals(13, 42, 0.32),
      ),
      (
        currentPositionAmount.sale.averages(10, 3.00, 0.02),
        nonDayTradeAmount.purchase.averages(3, 4.00, 0.04),
        expectedSwingTradeResult.averages(3, 4.00, 0.04, 3.00, 0.02),
        expectedUpdatedPositionAmount.sale.averages(7, 3.00, 0.02),
      ),
      (
        currentPositionAmount.sale.totals(10, 30.00, 0.20),
        nonDayTradeAmount.purchase.totals(10, 20.00, 0.30),
        expectedSwingTradeResult.totals(10, 20.00, 0.30, 30.00, 0.20),
        expectedUpdatedPositionAmount.Zero,
      ),
      (
        currentPositionAmount.sale.averages(10, 3.00, 0.02),
        nonDayTradeAmount.purchase.averages(12, 4.00, 0.03),
        expectedSwingTradeResult.averages(10, 4.00, 0.03, 3.00, 0.02),
        expectedUpdatedPositionAmount.purchase.averages(2, 4.00, 0.03),
      ),
    )

    forAll(cases) { case (currentPositionAmount, nonDayTradeAmount, expectedSwingTradeResult, expectedUpdatedPositionAmount) =>
      val (updatedPositionAmount, swingTradeResult) = Amount.combine(currentPositionAmount, nonDayTradeAmount)
      swingTradeResult should equal (expectedSwingTradeResult)
      updatedPositionAmount should equal (expectedUpdatedPositionAmount)
    }
  }

  test("PurchaseAmount accessors") {
    val amount = PurchaseAmount.fromAverages(2, 3.0, 0.5)

    amount.averagePriceWithCost should equal (3.5)
    amount.signedAveragePrice should equal (3.0)
  }

  test("SaleAmount accessors") {
    val amount = SaleAmount.fromAverages(2, 3.0, 0.5)

    amount.averagePriceWithCost should equal (2.5)
    amount.signedAveragePrice should equal (-3.0)
  }
}

object AmountTest {
  object DSL {
    val Zero: PurchaseAmount = PurchaseAmount.Zero
    val purchase: PurchaseDSL.type = PurchaseDSL
    val sale: SaleDSL.type = SaleDSL
  }

  object PurchaseDSL {
    val Zero: PurchaseAmount = PurchaseAmount.Zero
    def averages: (Int, Double, Double) => PurchaseAmount = PurchaseAmount.fromAverages
    def totals: (Int, Double, Double) => PurchaseAmount = PurchaseAmount.fromTotals
  }

  object SaleDSL {
    val Zero: SaleAmount = SaleAmount.Zero
    def averages: (Int, Double, Double) => SaleAmount = SaleAmount.fromAverages
    def totals: (Int, Double, Double) => SaleAmount = SaleAmount.fromTotals
  }
}
