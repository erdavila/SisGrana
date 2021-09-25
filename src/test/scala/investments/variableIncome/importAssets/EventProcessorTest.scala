package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.Amount

class EventProcessorTest extends TestBase {
  test("EventProcessor.processConversion()") {
    val cases = Table(
      (
        "conversion",
        "positions",
        "expected outcomes",
      ),
      (
        Event.Conversion("X", 1, "X", 4),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(10, 40.00, 0.40),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.fromSignedQuantityAndTotals(40, 40.00, 0.40),
            convertedToAsset = "X",
            convertedToQuantity = 40.0,
          ),
        ),
      ),
      (
        Event.Conversion("X", 1, "X", 4),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(-10, -40.00, 0.40),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.fromSignedQuantityAndTotals(-40, -40.00, 0.40),
            convertedToAsset = "X",
            convertedToQuantity = -40.0,
          ),
        ),
      ),
      (
        Event.Conversion("X", 4, "X", 1),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(16, 40.00, 0.40),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.fromSignedQuantityAndTotals(4, 40.00, 0.40),
            convertedToAsset = "X",
            convertedToQuantity = 4.0,
          ),
        ),
      ),
      (
        Event.Conversion("X", 4, "X", 1),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(15, 1.00, 0.10),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.fromSignedQuantityAndAverages(3, 4.00, 0.40),
            convertedToAsset = "X",
            convertedToQuantity = 3.75,
          ),
        ),
      ),
      (
        Event.Conversion("X", 2, "Y", 3),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(14, 1.50, 0.15),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.Zero,
            convertedToAsset = "Y",
            convertedToQuantity = 21,
          ),
          "Y" -> EventOutcome.AddToPosition(Amount.fromSignedQuantityAndAverages(21, 1.50 * 2 / 3, 0.15 * 2 / 3)),
        ),
      ),
      (
        Event.Conversion("X", 2, "Y", 3),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(15, 1.50, 0.15),
        ),
        Map(
          "X" -> EventOutcome.SetPosition(
            Amount.Zero,
            convertedToAsset = "Y",
            convertedToQuantity = 22.5,
          ),
          "Y" -> EventOutcome.AddToPosition(Amount.fromSignedQuantityAndAverages(22, 1.50 * 2 / 3, 0.15 * 2 / 3)),
        ),
      ),
    )

    forAll(cases) { case (conversion, positions, expectedOutcomes) =>
      val outcomes = EventProcessor.processConversion(conversion, positions)

      outcomes should equal (expectedOutcomes)
    }
  }

  test("EventProcessor.processBonus()") {
    val cases = Table(
      (
        "bonus",
        "positions",
        "expected outcomes",
      ),
      (
        Event.Bonus("X", 10, "X", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(20, 2.00, 0.02),
        ),
        Map(
          "X" -> EventOutcome.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
        ),
      ),
      (
        Event.Bonus("X", 10, "X", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(-20, 2.00, 0.02),
        ),
        Map.empty,
      ),
      (
        Event.Bonus("X", 10, "X", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(21, 2.00, 0.02),
        ),
        Map(
          "X" -> EventOutcome.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
        ),
      ),
      (
        Event.Bonus("X", 10, "Y", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(20, 2.00, 0.02),
        ),
        Map(
          "Y" -> EventOutcome.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
        ),
      ),
    )

    forAll(cases) { case (bonus, positions, expectedOutcomes) =>
      val outcomes = EventProcessor.processBonus(bonus, positions)

      outcomes should equal (expectedOutcomes)
    }
  }
}
