package sisgrana
package investments.variableIncome.model

import java.time.LocalDate

class AssetChangeTest extends TestBase {
  private val ZeroAssetChange = AssetChange.withZeroes("asset", "stockbroker", LocalDate.now())

  test("previousPosition, withPreviousPosition(), and fields") {
    val cases = Table(
      "previousPosition",
      PurchaseAmountWithCost.Zero,
      PurchaseAmountWithCost.fromAverages(10, 20.00, 0.10),
      SaleAmountWithCost.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { previousPosition =>
      val assetChange = ZeroAssetChange.withPreviousPosition(previousPosition)

      assetChange.previousPosition should equal (previousPosition)
      assetChange.previousPositionQuantity should equal (previousPosition.signedQuantity)
      assetChange.previousPositionAveragePrice should equal (previousPosition.averagePrice)
      assetChange.previousPositionAverageCost should equal (previousPosition.averageCost)
    }
  }

  test("None: eventEffect, withEventEffect(), and fields") {
    val assetChange = ZeroAssetChange.withEventEffect(None)

    assetChange.eventEffect shouldBe empty
    assetChange.eventSetPosition should equal (PurchaseAmountWithCost.Zero)
    assetChange.eventIncreaseAmount should equal (PurchaseAmountWithCost.Zero)
    assetChange.eventDecreaseAmount should equal (SaleAmountWithCost.Zero)
    assetChange.eventEffectType shouldBe empty
    assetChange.eventSetPositionQuantity should equal (0)
    assetChange.eventSetPositionAveragePrice should equal (0.0)
    assetChange.eventSetPositionAverageCost should equal (0.0)
    assetChange.eventIncreaseQuantity should equal (0)
    assetChange.eventIncreaseAveragePrice should equal (0.0)
    assetChange.eventIncreaseAverageCost should equal (0.0)
    assetChange.eventDecreaseQuantity should equal (0)
    assetChange.eventDecreaseAveragePrice should equal (0.0)
    assetChange.eventDecreaseAverageCost should equal (0.0)
  }

  test("eventSetPosition, eventEffect, withEventEffect(), and fields") {
    val cases = Table(
      "eventEffect",
      EventEffect.SetPosition(PurchaseAmountWithCost.Zero),
      EventEffect.SetPosition(PurchaseAmountWithCost.fromAverages(10, 10.00, 0.10)),
      EventEffect.SetPosition(SaleAmountWithCost.fromAverages(10, 10.00, 0.10)),
    )

    forAll(cases) { eventEffect =>
      val assetChange = ZeroAssetChange.withEventEffect(Some(eventEffect))

      assetChange.eventEffect should contain (eventEffect)
      assetChange.eventSetPosition should equal (eventEffect.position)
      assetChange.eventEffectType should contain (EventEffect.SetPosition.Type)
      assetChange.eventSetPositionQuantity should equal (eventEffect.position.signedQuantity)
      assetChange.eventSetPositionAveragePrice should equal (eventEffect.position.averagePrice)
      assetChange.eventSetPositionAverageCost should equal (eventEffect.position.averageCost)
    }
  }

  test("eventIncreaseAmount, eventDecreaseAmount, eventEffect, withEventEffect(), and fields") {
    val cases = Table(
      "eventEffect",
      EventEffect.AddToPosition(
        PurchaseAmountWithCost.Zero,
        SaleAmountWithCost.Zero,
      ),
      EventEffect.AddToPosition(
        PurchaseAmountWithCost.fromAverages(10, 10.00, 0.10),
        SaleAmountWithCost.fromAverages(7, 7.00, 0.70),
      ),
    )

    forAll(cases) { eventEffect =>
      val assetChange = ZeroAssetChange.withEventEffect(Some(eventEffect))

      assetChange.eventEffect should contain (eventEffect)
      assetChange.eventIncreaseAmount should equal (eventEffect.increaseAmount)
      assetChange.eventDecreaseAmount should equal (eventEffect.decreaseAmount)
      assetChange.eventEffectType should contain (EventEffect.AddToPosition.Type)
      assetChange.eventIncreaseQuantity should equal (eventEffect.increaseAmount.quantity)
      assetChange.eventIncreaseAveragePrice should equal (eventEffect.increaseAmount.averagePrice)
      assetChange.eventIncreaseAverageCost should equal (eventEffect.increaseAmount.averageCost)
      assetChange.eventDecreaseQuantity should equal (eventEffect.decreaseAmount.quantity)
      assetChange.eventDecreaseAveragePrice should equal (eventEffect.decreaseAmount.averagePrice)
      assetChange.eventDecreaseAverageCost should equal (eventEffect.decreaseAmount.averageCost)
    }
  }

  test("eventTradeResult and nonTradeEventPosition") {
    val cases = Table(
      (
        "previousPosition",
        "eventIncreaseAmount",
        "eventDecreaseAmount",
        "expected eventTradeResult",
        "expected nonTradeEventPosition",
      ),
      (
        AmountWithCost.Zero,
        PurchaseAmountWithCost.Zero,
        SaleAmountWithCost.Zero,
        TradeResult.Zero,
        AmountWithCost.Zero,
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(10, 10.00, 0.10),
        PurchaseAmountWithCost.Zero,
        SaleAmountWithCost.Zero,
        TradeResult.Zero,
        AmountWithCost.fromSignedQuantityAndAverages(10, 10.00, 0.10),
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(3, 4.00, 0.04),
        PurchaseAmountWithCost.fromAverages(7, 2.00, 0.02),
        SaleAmountWithCost.fromAverages(5, 3.00, 0.03),
        TradeResult.fromTotals(5, 13.00, 0.13, 15.00, 0.15),
        AmountWithCost.fromSignedQuantityAndTotals(5, 13.00, 0.13),
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(-1, 1.00, 0.01),
        PurchaseAmountWithCost.fromAverages(5, 5.00, 0.05),
        SaleAmountWithCost.fromAverages(4, 4.00, 0.04),
        TradeResult.fromTotals(5, 25.00, 0.25, 17.00, 0.17),
        AmountWithCost.Zero,
      ),
    )

    forAll(cases) { case (previousPosition, eventIncreaseAmount, eventDecreaseAmount, expectedEventTradeResult, expectedNonTradeEventPosition) =>
      val assetChange = ZeroAssetChange
        .withPreviousPosition(previousPosition)
        .withEventEffect(Some(EventEffect.AddToPosition(eventIncreaseAmount, eventDecreaseAmount)))

      assetChange.eventTradeResult should equal (expectedEventTradeResult)
      assetChange.nonTradeEventPosition should equal (expectedNonTradeEventPosition)
    }
  }

  test("postEventPosition, and fields") {
    val cases = Table(
      (
        "previousPosition",
        "eventEffect",
        "expected postEventPosition",
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(10, 10.0, 0.10),
        None,
        AmountWithCost.fromSignedQuantityAndAverages(10, 10.0, 0.10),
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(10, 10.0, 0.10),
        Some(EventEffect.SetPosition(AmountWithCost.fromSignedQuantityAndAverages(3, 3.33, 0.33))),
        AmountWithCost.fromSignedQuantityAndAverages(3, 3.33, 0.33),
      ),
      (
        AmountWithCost.fromSignedQuantityAndAverages(10, 1.00, 0.10),
        Some(EventEffect.AddToPosition(
          PurchaseAmountWithCost.fromAverages(1, 1.00, 0.10),
          SaleAmountWithCost.fromAverages(7, 1.00, 0.10),
        )),
        AmountWithCost.fromSignedQuantityAndAverages(4, 1.00, 0.10),
      ),
    )

    forAll(cases) { case (previousPosition, eventEffect, expectedPostEventPosition) =>
      val assetChange = ZeroAssetChange
        .withPreviousPosition(previousPosition)
        .withEventEffect(eventEffect)

      assetChange.postEventPosition should equal (expectedPostEventPosition)
    }
  }

  test("purchaseAmount, withPurchaseAmount(), and fields") {
    val cases = Table(
      "purchaseAmount",
      PurchaseAmountWithCost.Zero,
      PurchaseAmountWithCost.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { purchaseAmount =>
      val assetChange = ZeroAssetChange.withPurchaseAmount(purchaseAmount)

      assetChange.purchaseAmount should equal (purchaseAmount)
      assetChange.purchaseQuantity should equal (purchaseAmount.quantity)
      assetChange.purchaseAveragePrice should equal (purchaseAmount.averagePrice)
      assetChange.purchaseAverageCost should equal (purchaseAmount.averageCost)
    }
  }

  test("saleAmount, withSaleAmount(), and fields") {
    val cases = Table(
      "saleAmount",
      SaleAmountWithCost.Zero,
      SaleAmountWithCost.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { saleAmount =>
      val assetChange = ZeroAssetChange.withSaleAmount(saleAmount)

      assetChange.saleAmount should equal (saleAmount)
      assetChange.saleQuantity should equal (saleAmount.quantity)
      assetChange.saleAveragePrice should equal (saleAmount.averagePrice)
      assetChange.saleAverageCost should equal (saleAmount.averageCost)
    }
  }

  test("dayTradeResult and nonDayTradeOperationsAmount") {
    val expectedDayTradeResult = TradeResultTest.DSL
    val expectedNonDayTradeOperationsAmount = AmountWithCostTest.DSL

    val cases = Table(
      (
        "purchaseAmount",
        "saleAmount",
        "expected dayTradeResult",
        "expected nonDayTradeOperationsAmount",
      ),
      (
        PurchaseAmountWithCost.Zero,
        SaleAmountWithCost.Zero,
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.Zero,
      ),
      (
        PurchaseAmountWithCost.fromAverages(10, 10.00, 0.10),
        SaleAmountWithCost.Zero,
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.purchase.averages(10, 10.00, 0.10),
      ),
      (
        PurchaseAmountWithCost.Zero,
        SaleAmountWithCost.fromAverages(10, 10.00, 0.10),
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.sale.averages(10, 10.00, 0.10),
      ),
      (
        PurchaseAmountWithCost.fromAverages(10, 10.00, 0.10),
        SaleAmountWithCost.fromAverages(3, 11.00, 0.11),
        expectedDayTradeResult.averages(3, 10.00, 0.10, 11.00, 0.11),
        expectedNonDayTradeOperationsAmount.purchase.averages(7, 10.00, 0.10),
      ),
      (
        PurchaseAmountWithCost.fromAverages(3, 11.00, 0.11),
        SaleAmountWithCost.fromAverages(10, 10.00, 0.10),
        expectedDayTradeResult.averages(3, 11.00, 0.11, 10.00, 0.10),
        expectedNonDayTradeOperationsAmount.sale.averages(7, 10.00, 0.10),
      ),
      (
        PurchaseAmountWithCost.fromAverages(10, 10.00, 0.10),
        SaleAmountWithCost.fromAverages(10, 11.00, 0.11),
        expectedDayTradeResult.averages(10, 10.00, 0.10, 11.00, 0.11),
        expectedNonDayTradeOperationsAmount.Zero,
      ),
    )

    forAll(cases) { case (purchaseAmount, saleAmount, expectedDayTradeResult, expectedNonDayTradeOperationsAmount) =>
      val assetChange = ZeroAssetChange
        .withPurchaseAmount(purchaseAmount)
        .withSaleAmount(saleAmount)

      assetChange.dayTradeResult should equal (expectedDayTradeResult)
      assetChange.nonDayTradeOperationsAmount should equal (expectedNonDayTradeOperationsAmount)
    }
  }

  test("resultingPosition and operationsTradeResult") {
    val postEventPosition = AmountWithCostTest.DSL
    val nonDayTradeOperationsAmount = AmountWithCostTest.DSL
    val expectedResultingPosition = AmountWithCostTest.DSL
    val expectedOperationsTradeResult = TradeResultTest.DSL

    val cases = Table(
      (
        "postEventPosition",
        "nonDayTradeOperationsAmount",
        "expected resultingPosition",
        "expected operationsTradeResult",
      ),
      (
        postEventPosition.Zero,
        nonDayTradeOperationsAmount.Zero,
        expectedResultingPosition.Zero,
        expectedOperationsTradeResult.Zero,
      ),
      (
        postEventPosition.purchase.averages(10, 20.00, 0.20),
        nonDayTradeOperationsAmount.Zero,
        expectedResultingPosition.purchase.averages(10, 20.00, 0.20),
        expectedOperationsTradeResult.Zero,
      ),
    )

    forAll(cases) { case (postEventPosition, nonDayTradeOperationsAmount, expectedResultingPosition, expectedOperationsTradeResult) =>
      val ac0 = ZeroAssetChange.withEventEffect(Some(EventEffect.SetPosition(postEventPosition)))
      val assetChange =
        (nonDayTradeOperationsAmount: AmountWithCost) match {
          case p@PurchaseAmountWithCost(_, _, _) => ac0.withPurchaseAmount(p)
          case s@SaleAmountWithCost(_, _, _) => ac0.withSaleAmount(s)
        }

      assetChange.resultingPosition should equal (expectedResultingPosition)
      assetChange.operationsTradeResult should equal (expectedOperationsTradeResult)
    }
  }
}
