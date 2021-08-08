package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{Amount, StockbrokerAsset, TradeResult}
import java.time.LocalDate

class MainTest extends TestBase {
  test("events") {
    case class Case(
      previousPositions: Map[StockbrokerAsset, Amount],
      events: Seq[Event],
      expectedPostEventPositions: Map[StockbrokerAsset, Amount],
      expectedEventTradeResults: Map[StockbrokerAsset, TradeResult],
    )

    val cases = Table(
      "case",
      Case(
        previousPositions = Map(
          asset("X", "SB1") -> Amount.fromSignedQuantityAndTotals(3, 30.00, 0.30),
          asset("X", "SB2") -> Amount.fromSignedQuantityAndTotals(4, 20.00, 0.20),
        ),
        events = Seq(
          Event.Bonus("X", 2, "X", 1, 1.00),
        ),
        expectedPostEventPositions = Map(
          asset("X", "SB1") -> Amount.fromSignedQuantityAndTotals(4, 30.00 + 1.00, 0.30),
          asset("X", "SB2") -> Amount.fromSignedQuantityAndTotals(6, 20.00 + 2*1.00, 0.20),
        ),
        expectedEventTradeResults = Map(
          asset("X", "SB1") -> TradeResult.Zero,
          asset("X", "SB2") -> TradeResult.Zero,
        ),
      ),
      Case(
        previousPositions = Map(
          asset("X") -> Amount.fromSignedQuantityAndAverages(-3, 2.00, 0.02),
          asset("Y") -> Amount.fromSignedQuantityAndAverages(5, 15.00, 0.15),
        ),
        events = Seq(
          Event.Conversion("X", 1, "Z", 1),
          Event.Bonus("Y", 1, "Z", 1, 20.00),
        ),
        expectedPostEventPositions = Map(
          asset("X") -> Amount.Zero,
          asset("Z") -> Amount.fromSignedQuantityAndAverages(2, 20.00, 0.00),
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
      previousPositions: Map[StockbrokerAsset, Amount],
      brokerageNotes: Seq[BrokerageNote],
      expectedResultingPositions: Map[StockbrokerAsset, Amount],
      expectedDayTradeResults: Map[StockbrokerAsset, TradeResult] = Map.empty,
      expectedOperationsTradeResults: Map[StockbrokerAsset, TradeResult] = Map.empty,
    )

    val cases = Table(
      "case",
      Case(
        previousPositions = Map(
          asset("X") -> Amount.fromSignedQuantityAndAverages(10, 10.00, 0.10),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Sale, "X", 15, 15.00, None),
              Negotiation(Operation.Purchase, "Y", 20, 3.00, None),
              Negotiation(Operation.Sale, "Y", 8, 4.00, None),
            ),
            totalCost = 3.17,
          ),
        ),
        expectedResultingPositions = Map(
          asset("X") -> Amount.fromSignedQuantityAndAverages(-5, 15.00,0.15),
          asset("Y") -> Amount.fromSignedQuantityAndAverages(20 - 8, 3.00,0.03),
        ),
        expectedDayTradeResults = Map(
          asset("Y") -> TradeResult.fromAverages(8, 3.00, 0.03, 4.00, 0.04),
        ),
        expectedOperationsTradeResults = Map(
          asset("X") -> TradeResult.fromAverages(10, 10.00, 0.10, 15.00, 0.15),
        ),
      ),
      // purchased Call -> exercise is a purchase
      Case(
        previousPositions = Map(
          asset("zzzzA100") -> Amount.fromSignedQuantityAndAverages(4, 1.00, 0.01),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Purchase, "zzzz3", 3, 10.00, Some("zzzzA100")),
            ),
            totalCost = 0.30,
          ),
        ),
        expectedResultingPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(3, 10.00 + 1.00, 0.30/3 + 0.01),
          asset("zzzzA100") -> Amount.fromSignedQuantityAndAverages(1, 1.00, 0.01),
        ),
      ),
      // sold Call -> exercise is a sale
      Case(
        previousPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(5, 2.00, 0.02),
          asset("zzzzL100") -> Amount.fromSignedQuantityAndAverages(-4, 1.00, 0.01),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Sale, "zzzz3", 3, 10.00, Some("zzzzL100")),
            ),
            totalCost = 3.00,
          ),
        ),
        expectedResultingPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(2, 2.00, 0.02),
          asset("zzzzL100") -> Amount.fromSignedQuantityAndAverages(-1, 1.00, 0.01),
        ),
        expectedOperationsTradeResults = Map(
          asset("zzzz3") -> TradeResult.fromAverages(3, 2.00, 0.02, 10.00 + 1.00, 3.00 / 3 + 0.01),
        ),
      ),
      // purchased Put -> exercise is a sale
      Case(
        previousPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(5, 2.00, 0.02),
          asset("zzzzM100") -> Amount.fromSignedQuantityAndAverages(4, 1.00, 0.01),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Sale, "zzzz3", 3, 10.00, Some("zzzzM100")),
            ),
            totalCost = 3.00,
          ),
        ),
        expectedResultingPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(2, 2.00, 0.02),
          asset("zzzzM100") -> Amount.fromSignedQuantityAndAverages(1, 1.00, 0.01),
        ),
        expectedOperationsTradeResults = Map(
          asset("zzzz3") -> TradeResult.fromAverages(3, 2.00, 0.02, 10.00 - 1.00, 3.00 / 3 + 0.01),
        ),
      ),
      // sold Put -> exercise is a purchase
      Case(
        previousPositions = Map(
          asset("zzzzX100") -> Amount.fromSignedQuantityAndAverages(-4, 1.00, 0.01),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Purchase, "zzzz3", 3, 10.00, Some("zzzzX100")),
            ),
            totalCost = 3.00,
          ),
        ),
        expectedResultingPositions = Map(
          asset("zzzz3") -> Amount.fromSignedQuantityAndAverages(3, 10.00 - 1.00, 1.00 + 0.01),
          asset("zzzzX100") -> Amount.fromSignedQuantityAndAverages(-1, 1.00, 0.01),
        ),
      ),
      Case(
        previousPositions = Map(
          asset("zzzzA100") -> Amount.fromSignedQuantityAndAverages(3, 1.00, 0.10),
          asset("zzzzA110") -> Amount.fromSignedQuantityAndAverages(8, 1.10, 0.11),
        ),
        brokerageNotes = Seq(
          brokerageNote(
            negotiations = List(
              Negotiation(Operation.Purchase, "zzzz3", 3, 10.00, Some("zzzzA100")),
              Negotiation(Operation.Purchase, "zzzz3", 4, 11.00, Some("zzzzA110")),
              Negotiation(Operation.Sale, "zzzzA110", 4, 2.00, None),
            ),
            totalCost = 0.82,
          ),
        ),
        expectedResultingPositions = Map(
          asset("zzzzA100") -> Amount.Zero,
          asset("zzzzA110") -> Amount.Zero,
          asset("zzzz3") -> Amount.fromSignedQuantityAndTotals(
            signedQuantity = 3 + 4,
            totalValue = 3 * (10.00 + 1.00) + 4 * (11.00 + 1.10),
            totalCost = 3 * (0.10 + 0.10) + 4 * (0.11 + 0.11),
          ),
        ),
        expectedOperationsTradeResults = Map(
          asset("zzzzA110") -> TradeResult.fromAverages(4, 1.10, 0.11, 2.00, 0.02),
        ),
      ),
    )

    forAll(cases) { c =>
      val assetChanges =
        Main.processDateChanges(DefaultDate, c.previousPositions, Seq.empty, c.brokerageNotes)
          .map(ac => ac.stockbrokerAsset -> ac)
          .toMap

      assetChanges.keySet should equal (c.expectedResultingPositions.keySet)
      c.expectedDayTradeResults.keySet shouldBe subsetOf (assetChanges.keySet)
      c.expectedOperationsTradeResults.keySet shouldBe subsetOf (assetChanges.keySet)

      for (ac <- assetChanges.values) {
        withClue(s"resultingPosition for ${ac.stockbrokerAsset}:") {
          ac.resultingPosition should equal (c.expectedResultingPositions(ac.stockbrokerAsset))
        }
        withClue(s"dayTradeResult for ${ac.stockbrokerAsset}:") {
          val expectedDayTradeResult = c.expectedDayTradeResults.getOrElse(ac.stockbrokerAsset, TradeResult.Zero)
          ac.dayTradeResult should equal (expectedDayTradeResult)
        }
        withClue(s"operationsTradeResult for ${ac.stockbrokerAsset}:") {
          val expectedOperationsTradeResults = c.expectedOperationsTradeResults.getOrElse(ac.stockbrokerAsset, TradeResult.Zero)
          ac.operationsTradeResult should equal (expectedOperationsTradeResults)
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
