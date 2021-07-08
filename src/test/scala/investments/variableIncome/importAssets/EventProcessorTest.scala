package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.importAssets.Event.AveragePriceDefinition.{Constant, Multiplier}
import investments.variableIncome.importAssets.Event.{From, To}
import investments.variableIncome.model.{PurchaseAmountWithCost, SaleAmountWithCost, TradeResult}
import java.time.LocalDate
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class EventProcessorTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("calculateUpdatedPositions()") {
    val Asset = "ASST3"

    val cases = Table(
      (
        "event",
        "positionAndSwingTradeResultByAsset",
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
          Asset -> ((PurchaseAmountWithCost(5, 10.00, 0.10), TradeResult.Zero)),
        ),
        Some(
          Map(
            Asset -> ((PurchaseAmountWithCost(20, 10.00, 0.10), TradeResult.Zero)),
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
          Asset -> ((PurchaseAmountWithCost(25, 250.00, 0.50), TradeResult(7, 123.00, 0.32))),
        ),
        Some(
          Map(
            Asset -> ((PurchaseAmountWithCost(27, 260.00, 0.50), TradeResult(7, 123.00, 0.32))),
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
          Asset -> ((PurchaseAmountWithCost.fromAverages(3, 10.00, 0.10), TradeResult.Zero)),
          "XXXX4" -> ((SaleAmountWithCost.fromAverages(5, 5.00, 0.04), TradeResult(2, -14.00, 0.14))),
        ),
        Some(
          Map(
            Asset -> ((PurchaseAmountWithCost.Zero, TradeResult.Zero)),
            "XXXX4" -> ((SaleAmountWithCost.fromAverages(2, 5.00, 0.04), TradeResult(5, -29.00, 0.56))),
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
          Asset -> ((PurchaseAmountWithCost.fromAverages(3, 10.00, 0.30), TradeResult.Zero)),
          "XXXX4" -> ((SaleAmountWithCost.fromAverages(2, 8.00, 0.08), TradeResult(2, -14.00, 0.14))),
        ),
        Some(
          Map(
            Asset -> ((PurchaseAmountWithCost.Zero, TradeResult.Zero)),
            "XXXX4" -> ((PurchaseAmountWithCost.fromAverages(1, 10.00, 0.30), TradeResult(4, -18.00, 0.9))),
          )
        ),
      ),
    )

    forAll(cases) { case (event, positionAndSwingTradeResultByAsset, expectedPositionAndSwingTradeResultByAsset) =>
      val processor = new EventProcessor(event, LocalDate.now())
      val result = processor.calculateUpdatedPositions(positionAndSwingTradeResultByAsset)
      result should equal (expectedPositionAndSwingTradeResultByAsset)
    }
  }
}
