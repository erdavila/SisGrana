package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{AmountWithCostTest, TradeResultTest}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AssetOperationsProcessorTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("calculateSwingTradeResultAndUpdatedPositionAmount()") {
    val currentPositionAmount = AmountWithCostTest.DSL
    val nonDayTradeAmount = AmountWithCostTest.DSL
    val expectedSwingTradeResult = TradeResultTest.DSL
    val expectedUpdatedPositionAmount = AmountWithCostTest.DSL

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
        nonDayTradeAmount.purchase(10, 30.00, 0.20),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.Zero,
        nonDayTradeAmount.sale(10, 30.00, 0.20),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.purchase(10, 30.00, 0.20),
        nonDayTradeAmount.Zero,
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.purchase(10, 30.00, 0.20),
        nonDayTradeAmount.purchase(3, 12.00, 0.12),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.purchase(13, 42.00, 0.32),
      ),
      (
        currentPositionAmount.purchase(10, 30.00, 0.20),
        nonDayTradeAmount.sale(3, 12.00, 0.12),
        expectedSwingTradeResult(3, 3.00, 0.18),
        expectedUpdatedPositionAmount.purchase(7, 21.00, 0.14),
      ),
      (
        currentPositionAmount.purchase(10, 30.00, 0.20),
        nonDayTradeAmount.sale(10, 20.00, 0.12),
        expectedSwingTradeResult(10, -10.00, 0.32),
        expectedUpdatedPositionAmount.Zero,
      ),
      (
        currentPositionAmount.purchase(10, 30.00, 0.20),
        nonDayTradeAmount.sale(12, 48.00, 0.36),
        expectedSwingTradeResult(10, 10.00, 0.50),
        expectedUpdatedPositionAmount.sale(2, 8.00, 0.06),
      ),
      (
        currentPositionAmount.sale(10, 30.00, 0.20),
        nonDayTradeAmount.Zero,
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale(10, 30.00, 0.20),
      ),
      (
        currentPositionAmount.sale(10, 30.00, 0.20),
        nonDayTradeAmount.sale(3, 12.00, 0.12),
        expectedSwingTradeResult.Zero,
        expectedUpdatedPositionAmount.sale(13, 42, 0.32),
      ),
      (
        currentPositionAmount.sale(10, 30.00, 0.20),
        nonDayTradeAmount.purchase(3, 12.00, 0.12),
        expectedSwingTradeResult(3, -3.00, 0.18),
        expectedUpdatedPositionAmount.sale(7, 21.00, 0.14),
      ),
      (
        currentPositionAmount.sale(10, 30.00, 0.20),
        nonDayTradeAmount.purchase(10, 20.00, 0.30),
        expectedSwingTradeResult(10, 10.00, 0.50),
        expectedUpdatedPositionAmount.Zero,
      ),
      (
        currentPositionAmount.sale(10, 30.00, 0.20),
        nonDayTradeAmount.purchase(12, 48.00, 0.36),
        expectedSwingTradeResult(10, -10.00, 0.50),
        expectedUpdatedPositionAmount.purchase(2, 8.00, 0.06),
      ),
    )

    forAll(cases) { case (currentPositionAmount, nonDayTradeAmount, expectedSwingTradeResult, expectedUpdatedPositionAmount) =>
      val (swingTradeResult, updatedPositionAmount) = AssetOperationsProcessor.calculateSwingTradeResultAndUpdatedPosition(currentPositionAmount, nonDayTradeAmount)
      swingTradeResult should equal (expectedSwingTradeResult)
      updatedPositionAmount should equal (expectedUpdatedPositionAmount)
    }
  }
}
