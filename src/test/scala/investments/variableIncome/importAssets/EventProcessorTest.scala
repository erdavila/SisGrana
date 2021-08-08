package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{Amount, EventEffect}

class EventProcessorTest extends TestBase {
  test("EventProcessor.processConversion()") {
    val cases = Table(
      (
        "conversion",
        "positions",
        "expected effects",
      ),
      (
        Event.Conversion("X", 1, "X", 4),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(10, 40.00, 0.40),
        ),
        Map(
          "X" -> EventEffect.SetPosition(Amount.fromSignedQuantityAndTotals(40, 40.00, 0.40)),
        ),
      ),
      (
        Event.Conversion("X", 1, "X", 4),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(-10, -40.00, 0.40),
        ),
        Map(
          "X" -> EventEffect.SetPosition(Amount.fromSignedQuantityAndTotals(-40, -40.00, 0.40)),
        ),
      ),
      (
        Event.Conversion("X", 4, "X", 1),
        Map(
          "X" -> Amount.fromSignedQuantityAndTotals(16, 40.00, 0.40),
        ),
        Map(
          "X" -> EventEffect.SetPosition(Amount.fromSignedQuantityAndTotals(4, 40.00, 0.40)),
        ),
      ),
      (
        Event.Conversion("X", 4, "X", 1),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(15, 1.00, 0.10),
        ),
        Map(
          "X" -> EventEffect.SetPosition(Amount.fromSignedQuantityAndAverages(3, 4.00, 0.40)),
        ),
      ),
    )

    forAll(cases) { case (conversion, positions, expectedEffects) =>
      val effects = EventProcessor.processConversion(conversion, positions)

      effects should equal (expectedEffects)
    }
  }

  test("EventProcessor.processBonus()") {
    val cases = Table(
      (
        "bonus",
        "positions",
        "expected effects",
      ),
      (
        Event.Bonus("X", 10, "X", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(20, 2.00, 0.02),
        ),
        Map(
          "X" -> EventEffect.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
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
          "X" -> EventEffect.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
        ),
      ),
      (
        Event.Bonus("X", 10, "Y", 1, 1.00),
        Map(
          "X" -> Amount.fromSignedQuantityAndAverages(20, 2.00, 0.02),
        ),
        Map(
          "Y" -> EventEffect.AddToPosition(Amount.fromSignedQuantityAndAverages(2, 1.00, 0.00))
        ),
      ),
    )

    forAll(cases) { case (bonus, positions, expectedEffects) =>
      val effects = EventProcessor.processBonus(bonus, positions)

      effects should equal (expectedEffects)
    }
  }
}
