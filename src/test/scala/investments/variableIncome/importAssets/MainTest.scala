package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{AmountWithCost, StockbrokerAsset, TradeResult}
import java.time.LocalDate

class MainTest extends TestBase {
  test("events") {
    case class Case(
      previousPositions: Map[StockbrokerAsset, AmountWithCost],
      events: Seq[Event],
      expectedPostEventPositions: Map[StockbrokerAsset, AmountWithCost],
      expectedEventTradeResults: Map[StockbrokerAsset, TradeResult],
    )

    val cases = Table(
      "case",
      Case(
        previousPositions = Map(
          asset("X", "SB1") -> AmountWithCost.fromSignedQuantityAndTotals(3, 30.00, 0.30),
          asset("X", "SB2") -> AmountWithCost.fromSignedQuantityAndTotals(4, 20.00, 0.20),
        ),
        events = Seq(
          Event.Bonus("X", 2, "X", 1, 1.00),
        ),
        expectedPostEventPositions = Map(
          asset("X", "SB1") -> AmountWithCost.fromSignedQuantityAndTotals(4, 30.00 + 1.00, 0.30),
          asset("X", "SB2") -> AmountWithCost.fromSignedQuantityAndTotals(6, 20.00 + 2*1.00, 0.20),
        ),
        expectedEventTradeResults = Map(
          asset("X", "SB1") -> TradeResult.Zero,
          asset("X", "SB2") -> TradeResult.Zero,
        ),
      ),
      Case(
        previousPositions = Map(
          asset("X") -> AmountWithCost.fromSignedQuantityAndAverages(-3, 2.00, 0.02),
          asset("Y") -> AmountWithCost.fromSignedQuantityAndAverages(5, 15.00, 0.15),
        ),
        events = Seq(
          Event.Conversion("X", 1, "Z", 1),
          Event.Bonus("Y", 1, "Z", 1, 20.00),
        ),
        expectedPostEventPositions = Map(
          asset("X") -> AmountWithCost.Zero,
          asset("Z") -> AmountWithCost.fromSignedQuantityAndAverages(2, 20.00, 0.00),
        ),
        expectedEventTradeResults = Map(
          asset("X") -> TradeResult.Zero,
          asset("Z") -> TradeResult.fromAverages(3, 20.00, 0.00, 2.00, 0.02),
        )
      ),
    )

    forAll(cases) { c =>
      assert(c.expectedPostEventPositions.keySet == c.expectedEventTradeResults.keySet)

      val assetChanges =
        Main.processDateChanges(DefaultDate, c.previousPositions, c.events, Seq.empty)
          .map(ac => ac.stockbrokerAsset -> ac)
          .toMap

      assetChanges.keySet should equal (c.expectedPostEventPositions.keySet)
      for (ac <- assetChanges.values) {
        withClue(s"${ac.stockbrokerAsset}:") {
          ac.postEventPosition should equal (c.expectedPostEventPositions(ac.stockbrokerAsset))
          ac.eventTradeResult should equal (c.expectedEventTradeResults(ac.stockbrokerAsset))
        }
      }
    }
  }

  test("operations") {
    case class Case(
      previousPositions: Map[StockbrokerAsset, AmountWithCost],
      brokerageNotes: Seq[BrokerageNote],
      expectedResultingPosition: Map[StockbrokerAsset, AmountWithCost],
      expectedDayTradeResults: Map[StockbrokerAsset, TradeResult],
      expectedOperationsTradeResults: Map[StockbrokerAsset, TradeResult],
    )

    val cases = Table(
      "case",
      Case(
        previousPositions = Map(
          asset("X") -> AmountWithCost.fromSignedQuantityAndAverages(10, 10.00, 0.10),
        ),
        Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Sale, "X", 15, 15.00),
              Negotiation(Operation.Purchase, "Y", 20, 3.00),
              Negotiation(Operation.Sale, "Y", 8, 4.00),
            ),
            totalCost = 3.17,
          ),
        ),
        expectedResultingPosition = Map(
          asset("X") -> AmountWithCost.fromSignedQuantityAndAverages(-5, 15.00,0.15),
          asset("Y") -> AmountWithCost.fromSignedQuantityAndAverages(20 - 8, 3.00,0.03),
        ),
        expectedDayTradeResults = Map(
          asset("X") -> TradeResult.Zero,
          asset("Y") -> TradeResult.fromAverages(8, 3.00, 0.03, 4.00, 0.04),
        ),
        expectedOperationsTradeResults = Map(
          asset("X") -> TradeResult.fromAverages(10, 10.00, 0.10, 15.00, 0.15),
          asset("Y") -> TradeResult.Zero,
        ),
      ),
    )

    forAll(cases) { c =>
      assert(c.expectedResultingPosition.keySet == c.expectedOperationsTradeResults.keySet)

      val assetChanges =
        Main.processDateChanges(DefaultDate, c.previousPositions, Seq.empty, c.brokerageNotes)
          .map(ac => ac.stockbrokerAsset -> ac)
          .toMap

      assetChanges.keySet should equal (c.expectedResultingPosition.keySet)
      for (ac <- assetChanges.values) {
        withClue(s"resultingPosition for ${ac.stockbrokerAsset}:") {
          ac.resultingPosition should equal (c.expectedResultingPosition(ac.stockbrokerAsset))
        }
        withClue(s"dayTradeResult for ${ac.stockbrokerAsset}:") {
          ac.dayTradeResult should equal (c.expectedDayTradeResults(ac.stockbrokerAsset))
        }
        withClue(s"operationsTradeResult for ${ac.stockbrokerAsset}:") {
          ac.operationsTradeResult should equal (c.expectedOperationsTradeResults(ac.stockbrokerAsset))
        }
      }
    }
  }

  private val DefaultDate = LocalDate.now()
  private val DefaultStockBroker = "Default Stockbroker"

  private def asset(asset: String, stockbroker: String = DefaultStockBroker) =
    StockbrokerAsset(stockbroker, asset)

  private def brokerageNote(negotiations: List[Negotiation], totalCost: Double, stockbroker: String = DefaultStockBroker, date: LocalDate = DefaultDate) = {
    val costs = List(Cost("Default Cost", totalCost))
    val totalValue = negotiations
      .map(n =>
        n.operation match {
          case Operation.Purchase => -n.totalValue
          case Operation.Sale => n.totalValue
        }
      )
      .sum - totalCost
    BrokerageNote(stockbroker, date, negotiations, costs, totalValue)
  }
}
