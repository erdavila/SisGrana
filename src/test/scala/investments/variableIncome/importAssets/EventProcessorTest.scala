package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.Event.AveragePriceDefinition.{Constant, Multiplier}
import investments.variableIncome.importAssets.Event.{From, To}
import investments.variableIncome.importAssets.EventProcessor.EventOutcome
import investments.variableIncome.model.{PurchaseAmountWithCost, SaleAmountWithCost, TradeResult}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class EventProcessorTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("calculateOutcome()") {
    val Asset = "ASST3"

    val cases = Table(
      (
        "event",
        "positionByAsset",
        "expected updatedPositionAndSwingTradeResultByAsset",
      ),
      (
        Event(
          From(1, Asset),
          Vector(
            To(4, Asset, Multiplier(0.25)),
          )
        ),
        Map(
          Asset -> PurchaseAmountWithCost.fromTotals(5, 10.00, 0.10),
        ),
        Some(
          Map(
            Asset -> EventOutcome(TradeResult.Zero, PurchaseAmountWithCost.fromTotals(20, 10.00, 0.10)),
          )
        ),
      ),
      (
        Event(
          From(10, Asset),
          Vector(
            To(10, Asset, Multiplier(1)),
            To(1, Asset, Constant(5.0)),
          )
        ),
        Map(
          Asset -> PurchaseAmountWithCost.fromTotals(25, 250.00, 0.50),
        ),
        Some(
          Map(
            Asset -> EventOutcome(TradeResult.Zero, PurchaseAmountWithCost.fromTotals(27, 260.00, 0.50)),
          )
        ),
      ),
      (
        Event(
          From(1, Asset),
          Vector(
            To(1, "XXXX4", Multiplier(1)),
          )
        ),
        Map(
          Asset -> PurchaseAmountWithCost.fromAverages(3, 10.00, 0.10),
          "XXXX4" -> SaleAmountWithCost.fromAverages(5, 5.00, 0.08),
        ),
        Some(
          Map(
            Asset -> EventOutcome(TradeResult.Zero, PurchaseAmountWithCost.Zero),
            "XXXX4" -> EventOutcome(TradeResult(3, -15.00, 0.54), SaleAmountWithCost.fromAverages(2, 5.00, 0.08)),
          )
        ),
      ),
      (
        Event(
          From(1, Asset),
          Vector(
            To(1, "XXXX4", Multiplier(1)),
          )
        ),
        Map(
          Asset -> PurchaseAmountWithCost.fromAverages(3, 10.00, 0.30),
          "XXXX4" -> SaleAmountWithCost.fromAverages(2, 8.00, 0.08),
        ),
        Some(
          Map(
            Asset -> EventOutcome(TradeResult.Zero, PurchaseAmountWithCost.Zero),
            "XXXX4" -> EventOutcome(TradeResult(2, -4.00, 0.76), PurchaseAmountWithCost.fromAverages(1, 10.00, 0.30)),
          )
        ),
      ),
    )

    forAll(cases) { case (event, positionByAsset, expectedPositionAndSwingTradeResultByAsset) =>
      val processor = new EventProcessor(event)
      val result = processor.calculateOutcome(positionByAsset)
      result should equal (expectedPositionAndSwingTradeResultByAsset)
    }
  }
}
