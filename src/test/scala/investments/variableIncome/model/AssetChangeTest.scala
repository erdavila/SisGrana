package sisgrana
package investments.variableIncome.model

import java.time.LocalDate

class AssetChangeTest extends TestBase {
  private val ZeroAssetChange = AssetChange.withZeroes("asset", "stockbroker", LocalDate.now())

  test("withPreviousPosition(), previousPosition, and fields") {
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

  test("withEventTradeResult(), eventTradeResult, and fields") {
    val cases = Table(
      "eventTradeResult",
      TradeResult.Zero,
      TradeResult.fromTotals(10, 20.00, 0.20, 30.00, 0.30),
      TradeResult.fromTotals(10, 30.00, 0.30, 20.00, 0.20),
    )

    forAll(cases) { eventTradeResult =>
      val assetChange = ZeroAssetChange.withEventTradeResult(eventTradeResult)

      assetChange.eventTradeResult should equal (eventTradeResult)
      assetChange.eventTradeQuantity should equal (eventTradeResult.quantity)
      assetChange.eventTradeTotalPurchaseValue should equal (eventTradeResult.totalPurchaseValue)
      assetChange.eventTradeTotalPurchaseCost should equal (eventTradeResult.totalPurchaseCost)
      assetChange.eventTradeTotalSaleValue should equal (eventTradeResult.totalSaleValue)
      assetChange.eventTradeTotalSaleCost should equal (eventTradeResult.totalSaleCost)
    }
  }

  test("withPostEventPosition, postEventPosition, and fields") {
    val cases = Table(
      "postEventPosition",
      PurchaseAmountWithCost.Zero,
      PurchaseAmountWithCost.fromAverages(10, 20.00, 0.10),
      SaleAmountWithCost.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { postEventPosition =>
      val assetChange = ZeroAssetChange.withPostEventPosition(postEventPosition)

      assetChange.postEventPosition should equal (postEventPosition)
      assetChange.postEventPositionQuantity should equal (postEventPosition.signedQuantity)
      assetChange.postEventPositionAveragePrice should equal (postEventPosition.averagePrice)
      assetChange.postEventPositionAverageCost should equal (postEventPosition.averageCost)
    }
  }

  test("withPurchaseAmount, purchaseAmount, and fields") {
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

  test("withSaleAmount, saleAmount, and fields") {
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
      val ac0 = ZeroAssetChange.withPostEventPosition(postEventPosition)
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
