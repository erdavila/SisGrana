package sisgrana
package investments.variableIncome.model

import investments.variableIncome.importAssets.EventOutcome
import investments.variableIncome.model.AssetChangeTest.DSL._
import java.time.LocalDate
import org.scalatest.Inside.inside
import scala.language.implicitConversions
import utils.DateRangeTest.int2Date
import utils.{DateRange, sameNonZeroSigns}

class AssetChangeTest extends TestBase {
  private val ZeroAssetChange = AssetChange.withZeroes("asset", "stockbroker", LocalDate.now())

  test("previousPosition, withPreviousPosition(), and fields") {
    val cases = Table(
      "previousPosition",
      PurchaseAmount.Zero,
      PurchaseAmount.fromAverages(10, 20.00, 0.10),
      SaleAmount.fromAverages(10, 20.00, 0.10),
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
    assetChange.eventSetPosition should equal (PurchaseAmount.Zero)
    assetChange.eventIncreaseAmount should equal (PurchaseAmount.Zero)
    assetChange.eventDecreaseAmount should equal (SaleAmount.Zero)
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

  test("eventSetPosition, eventEffect, eventConvertedTo*, withEventEffect(), and fields") {
    val cases = Table(
      "eventEffect",
      EventEffect.SetPosition(PurchaseAmount.Zero),
      EventEffect.SetPosition(PurchaseAmount.fromAverages(10, 10.00, 0.10)),
      EventEffect.SetPosition(SaleAmount.fromAverages(10, 10.00, 0.10)),
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
        PurchaseAmount.Zero,
        SaleAmount.Zero,
      ),
      EventEffect.AddToPosition(
        PurchaseAmount.fromAverages(10, 10.00, 0.10),
        SaleAmount.fromAverages(7, 7.00, 0.70),
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
        Amount.Zero,
        PurchaseAmount.Zero,
        SaleAmount.Zero,
        TradeResult.Zero,
        Amount.Zero,
      ),
      (
        Amount.fromSignedQuantityAndAverages(10, 10.00, 0.10),
        PurchaseAmount.Zero,
        SaleAmount.Zero,
        TradeResult.Zero,
        Amount.fromSignedQuantityAndAverages(10, 10.00, 0.10),
      ),
      (
        Amount.fromSignedQuantityAndAverages(3, 4.00, 0.04),
        PurchaseAmount.fromAverages(7, 2.00, 0.02),
        SaleAmount.fromAverages(5, 3.00, 0.03),
        TradeResult.fromTotals(5, 13.00, 0.13, 15.00, 0.15),
        Amount.fromSignedQuantityAndTotals(5, 13.00, 0.13),
      ),
      (
        Amount.fromSignedQuantityAndAverages(-1, 1.00, 0.01),
        PurchaseAmount.fromAverages(5, 5.00, 0.05),
        SaleAmount.fromAverages(4, 4.00, 0.04),
        TradeResult.fromTotals(5, 25.00, 0.25, 17.00, 0.17),
        Amount.Zero,
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
        Amount.fromSignedQuantityAndAverages(10, 10.0, 0.10),
        None,
        Amount.fromSignedQuantityAndAverages(10, 10.0, 0.10),
      ),
      (
        Amount.fromSignedQuantityAndAverages(10, 10.0, 0.10),
        Some(EventEffect.SetPosition(Amount.fromSignedQuantityAndAverages(3, 3.33, 0.33))),
        Amount.fromSignedQuantityAndAverages(3, 3.33, 0.33),
      ),
      (
        Amount.fromSignedQuantityAndAverages(10, 1.00, 0.10),
        Some(EventEffect.AddToPosition(
          PurchaseAmount.fromAverages(1, 1.00, 0.10),
          SaleAmount.fromAverages(7, 1.00, 0.10),
        )),
        Amount.fromSignedQuantityAndAverages(4, 1.00, 0.10),
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
      PurchaseAmount.Zero,
      PurchaseAmount.fromAverages(10, 20.00, 0.10),
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
      SaleAmount.Zero,
      SaleAmount.fromAverages(10, 20.00, 0.10),
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
    val expectedNonDayTradeOperationsAmount = AmountTest.DSL

    val cases = Table(
      (
        "purchaseAmount",
        "saleAmount",
        "expected dayTradeResult",
        "expected nonDayTradeOperationsAmount",
      ),
      (
        PurchaseAmount.Zero,
        SaleAmount.Zero,
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.Zero,
      ),
      (
        PurchaseAmount.fromAverages(10, 10.00, 0.10),
        SaleAmount.Zero,
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.purchase.averages(10, 10.00, 0.10),
      ),
      (
        PurchaseAmount.Zero,
        SaleAmount.fromAverages(10, 10.00, 0.10),
        expectedDayTradeResult.Zero,
        expectedNonDayTradeOperationsAmount.sale.averages(10, 10.00, 0.10),
      ),
      (
        PurchaseAmount.fromAverages(10, 10.00, 0.10),
        SaleAmount.fromAverages(3, 11.00, 0.11),
        expectedDayTradeResult.averages(3, 10.00, 0.10, 11.00, 0.11),
        expectedNonDayTradeOperationsAmount.purchase.averages(7, 10.00, 0.10),
      ),
      (
        PurchaseAmount.fromAverages(3, 11.00, 0.11),
        SaleAmount.fromAverages(10, 10.00, 0.10),
        expectedDayTradeResult.averages(3, 11.00, 0.11, 10.00, 0.10),
        expectedNonDayTradeOperationsAmount.sale.averages(7, 10.00, 0.10),
      ),
      (
        PurchaseAmount.fromAverages(10, 10.00, 0.10),
        SaleAmount.fromAverages(10, 11.00, 0.11),
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
    val postEventPosition = AmountTest.DSL
    val nonDayTradeOperationsAmount = AmountTest.DSL
    val expectedResultingPosition = AmountTest.DSL
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
        (nonDayTradeOperationsAmount: Amount) match {
          case p@PurchaseAmount(_, _, _) => ac0.withPurchaseAmount(p)
          case s@SaleAmount(_, _, _) => ac0.withSaleAmount(s)
        }

      assetChange.resultingPosition should equal (expectedResultingPosition)
      assetChange.operationsTradeResult should equal (expectedOperationsTradeResult)
    }
  }

  test("convertedTo, withConvertedTo(), and fields") {
    val cases = Table(
      "convertedTo option",
      Some(ConvertedTo("Y", 10.0)),
      None
    )

    val NonZeroAssetChange = ZeroAssetChange
      .withPurchaseAmount(PurchaseAmount.fromAverages(5, 1.00, 0.01))

    forAll(cases) { convertedToOpt =>
      val assetChange = NonZeroAssetChange.withConvertedTo(convertedToOpt)

      assetChange.convertedTo should equal (convertedToOpt)
      assetChange.convertedToAsset.isDefined should equal (convertedToOpt.isDefined)
      assetChange.convertedToQuantity.isDefined should equal (convertedToOpt.isDefined)
      assetChange.convertedToAsset should equal (convertedToOpt.map(_.asset))
      assetChange.convertedToQuantity should equal (convertedToOpt.map(_.quantity))
    }
  }

  test(".toDateRanges()") {
    case class Case(
      assetChanges: Seq[AssetChange],
      expectedDayChangeDateRanges: Seq[DateRange],
      expectedFullDayDateRanges: Seq[DateRange],
    )

    object Case {
      def apply(assetChanges: Seq[AssetChange], expectedAnyModeDateRanges: Seq[DateRange]): Case =
        Case(assetChanges, expectedAnyModeDateRanges, expectedAnyModeDateRanges)
    }

    val BeforeMinDate = 1
    val MinDate = 3
    val MiddleDate = 5
    val MaxDate = 7
    val AfterMaxDate = 9

    def changes(
      beforeMinDateQuantity2: Option[Int],
      beforeMinDateQuantity1: Option[Int],
      minDateQuantity: Option[Int],
      middleDateQuantity: Option[Int],
      maxDateQuantity: Option[Int],
      afterMaxDateQuantity1: Option[Int],
      afterMaxDateQuantity2: Option[Int],
    ): Seq[AssetChange] =
      assetChangesFor()(seqBuilder =>
        Seq(
          (beforeMinDateQuantity2, BeforeMinDate - 1),
          (beforeMinDateQuantity1, BeforeMinDate),
          (minDateQuantity, MinDate),
          (middleDateQuantity, MiddleDate),
          (maxDateQuantity, MaxDate),
          (afterMaxDateQuantity1, AfterMaxDate),
          (afterMaxDateQuantity2, AfterMaxDate + 1),
        ).foldLeft(seqBuilder) {
          case (seqBuilder, (Some(quantity), dateOffset)) => seqBuilder.on(dateOffset).resultingQuantity(quantity)
          case (seqBuilder, (None, _)) => seqBuilder
        }
      )

    def ranges(pairs: (LocalDate, LocalDate)*): Seq[DateRange] =
      for ((b, e) <- pairs)
        yield DateRange(b, e)

    val Z = Some(0)
    val * = Some(999)
    val - = None

    val cases = Table(
      "case",

      //             min ↓     ↓ max
      Case(changes(-, Z, -, -, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, *, -, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, -, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, *, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, -, Z, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, *, -, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, -, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, -, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, -, *, -), expectedAnyModeDateRanges   = ranges()),
      //             min ↕     ↕ max
      Case(changes(Z, Z, -, -, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(Z, *, -, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(*, Z, -, -, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(*, *, -, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, Z, -, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, *, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, Z, -, -, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate))),
      Case(changes(-, *, *, -, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, -, Z, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, -, *, -, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, *, -, Z, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, *, -, *, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, -, -, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, -, -, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, *, -, -, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, -, -, *, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, -, -, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, -, -, -, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, *, -, -, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, -, -, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, Z, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, *, -, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, *, Z, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, -, *, *, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, -, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, -, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, *, -, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, -, *, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, -, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, -, -, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, *, -, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, -, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, -, Z, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, Z, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, *, Z, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, *, *, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, Z, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, Z, -, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, *, -, Z, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, *, -, *, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, -, Z, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, Z, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, *, Z, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, -, *, *, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, -, -, Z, Z), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, -, Z, *), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, -, *, Z), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, -, -, *, *), expectedAnyModeDateRanges   = ranges()),
      //             min ↕     ↕ max
      Case(changes(-, Z, Z, Z, -, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, Z, *, -, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, Z, *, Z, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, Z, *, *, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, Z, Z, -, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate))),
      Case(changes(-, *, Z, *, -, -, -), expectedDayChangeDateRanges = ranges((MiddleDate, MaxDate)), expectedFullDayDateRanges = ranges((MinDate, MinDate), (MiddleDate, MaxDate))),
      Case(changes(-, *, *, Z, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, *, *, *, -, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, Z, -, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, Z, -, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, Z, *, -, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, *, -, *, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, Z, -, Z, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate))),
      Case(changes(-, *, Z, -, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate), (MaxDate, MaxDate))),
      Case(changes(-, *, *, -, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, *, -, *, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, Z, -, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, Z, -, -, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, Z, *, -, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, Z, *, -, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, Z, -, -, Z, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate))),
      Case(changes(-, *, Z, -, -, *, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MinDate, MinDate))),
      Case(changes(-, *, *, -, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, *, *, -, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, Z, Z, -, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, Z, *, -, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, Z, *, Z, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, Z, *, *, -, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, *, Z, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, -, *, Z, *, -, -), expectedDayChangeDateRanges = ranges((MinDate, MiddleDate)), expectedFullDayDateRanges = ranges((MinDate, MiddleDate), (MaxDate, MaxDate))),
      Case(changes(-, -, *, *, Z, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, *, *, -, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, Z, -, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, Z, -, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, *, -, Z, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, Z, *, -, *, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, *, Z, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, -, *, Z, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MiddleDate))),
      Case(changes(-, -, *, *, -, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, *, -, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, Z, -, Z, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, -, Z, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, Z, -, *, Z, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, Z, -, *, *, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, *, -, Z, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, -, Z, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, -, *, Z, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, *, -, *, *, -), expectedAnyModeDateRanges   = ranges((MinDate, MaxDate))),
      Case(changes(-, -, -, Z, Z, Z, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, Z, Z, *, -), expectedAnyModeDateRanges   = ranges()),
      Case(changes(-, -, -, Z, *, Z, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, Z, *, *, -), expectedDayChangeDateRanges = ranges(), expectedFullDayDateRanges = ranges((MaxDate, MaxDate))),
      Case(changes(-, -, -, *, Z, Z, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, *, Z, *, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, *, *, Z, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      Case(changes(-, -, -, *, *, *, -), expectedAnyModeDateRanges   = ranges((MiddleDate, MaxDate))),
      //             min ↑     ↑ max
    )

    forAll(cases) { c =>
      def withMode(expectedDateRanges: Seq[DateRange])(implicit mode: DateRange.Mode): Unit =
        withClue(s"$mode mode") {
          val result = AssetChange.toDateRanges(c.assetChanges, MinDate, MaxDate)
          result.indexedSeq should equal (expectedDateRanges)
        }

      withMode(c.expectedDayChangeDateRanges)(DateRange.Mode.DayChange)
      withMode(c.expectedFullDayDateRanges)(DateRange.Mode.FullDay)
    }
  }

  //noinspection ZeroIndexToHead
  test("AssetChangesBuilder") {
    {
      val result = assetChangesFor()(_
        .on(1).resultingQuantity(3)
        .on(3).resultingQuantity(7)
      )

      result should have length 2

      result(0).date should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).previousPosition.quantity should equal (0)
      result(0).purchaseAmount.quantity should equal (3)
      result(0).resultingPosition.quantity should equal (3)

      result(1).date should equal (int2Date(3))
      result(1).previousPosition.quantity should equal (3)
      result(1).purchaseAmount.quantity should equal (4)
      result(1).resultingPosition.quantity should equal (7)
    }

    {
      val result = assetChangesFor("a1")(_
        .on(1).resultingQuantity(7)
        .on(3).eventConvertedToSame(14)
      )

      result should have length 2

      result(0).date should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (7)
      result(0).convertedTo should contain (ConvertedTo("a1", 14.0))

      result(1).date should equal (int2Date(3))
      inside(result(1).eventEffect) { case Some(sp@ EventEffect.SetPosition(_)) =>
        sp.position.quantity should equal (14)
      }
      result(1).resultingPosition.quantity should equal (14)
      result(1).convertedTo should be (empty)
    }

    {
      val result = assetChangesFor("a1")(_
        .on(1).resultingQuantity(7)
        .on(3).eventConvertedToOther("a2")
      )

      result should have length 2

      result(0).date should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (7)
      result(0).convertedTo should contain (ConvertedTo("a2", 7.0))

      result(1).date should equal (int2Date(3))
      result(1).previousPosition.quantity should equal (7)
      inside(result(1).eventEffect) { case Some(sp@ EventEffect.SetPosition(_)) =>
        sp.position.quantity should equal (0)
      }
      result(1).resultingPosition.quantity should equal (0)
      result(1).convertedTo should be (empty)
    }

    {
      val result = assetChangesFor()(_
        .on(1).resultingQuantity(3)
        .on(3).eventIncreasedQuantity(4)
        .on(5).eventIncreasedQuantity(2).resultingQuantity(10)
      )

      result should have length 3

      result(0).date should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (3)

      result(1).date should equal (int2Date(3))
      result(1).endDate should equal (int2Date(5))
      result(1).previousPosition.quantity should equal (3)
      inside(result(1).eventEffect) { case Some(atp@ EventEffect.AddToPosition(_, _)) =>
        atp.increaseAmount.quantity should equal (4)
      }
      result(1).resultingPosition.quantity should equal (7)

      result(2).date should equal (int2Date(5))
      result(2).previousPosition.quantity should equal (7)
      inside(result(2).eventEffect) { case Some(atp@ EventEffect.AddToPosition(_, _)) =>
        atp.increaseAmount.quantity should equal (2)
      }
      result(2).purchaseAmount.quantity should equal (1)
      result(2).resultingPosition.quantity should equal (10)
    }
  }
}

object AssetChangeTest {

  object DSL {
    val NonZero = 999

    case class AssetChangesBuilder(asset: String, stockbroker: String, assetChanges: Vector[AssetChange]) {
      def noChanges: AssetChangesBuilder = this

      def on(date: LocalDate): AssetChangeBuilder = {
        val (previousPosition, seqBuilder) = assetChanges match {
          case init :+ last => (last.resultingPosition, this.copy(assetChanges = init :+ last.copy(endDate = date)))
          case _ => (Amount.Zero, this)
        }

        val ac = AssetChange
          .withZeroes(asset, stockbroker, date)
          .withPreviousPosition(previousPosition)

        AssetChangeBuilder(seqBuilder, ac)
      }
    }

    case class AssetChangeBuilder(seqBuilder: AssetChangesBuilder, assetChange: AssetChange) {
      def eventConvertedToSame(quantity: Int): AssetChangeBuilder = {
        require(sameNonZeroSigns(assetChange.previousPosition.signedQuantity, quantity))
        require(seqBuilder.assetChanges.nonEmpty)

        val amount = Amount.fromSignedQuantityAndAverages(quantity, 1.00, 0.01)
        eventConvertedTo(amount, assetChange.asset, quantity)
      }

      def eventConvertedToOther(otherAsset: String): AssetChangeBuilder = {
        require(otherAsset != seqBuilder.asset)

        val quantity = assetChange.previousPosition.signedQuantity
        eventConvertedTo(Amount.Zero, otherAsset, quantity)
      }

      private def eventConvertedTo(amount: Amount, asset: String, quantity: Double): AssetChangeBuilder = {
        require(seqBuilder.assetChanges.nonEmpty)

        val previousACs = seqBuilder.assetChanges
        val newPreviousACs = previousACs.init :+ previousACs.last.withConvertedTo(Some(ConvertedTo(asset, quantity)))

        this.copy(
          seqBuilder = seqBuilder.copy(assetChanges = newPreviousACs),
          assetChange = assetChange.withEventEffect(Some(EventEffect.SetPosition(amount))),
        )
      }

      def eventIncreasedQuantity(quantity: Int): AssetChangeBuilder = {
        require(quantity != 0)
        val amount = Amount.fromSignedQuantityAndAverages(quantity, 1.00, 0.01)
        val newAc = assetChange.withEventEffect(Some(EventOutcome.AddToPosition(amount).toEffect))
        this.copy(assetChange = newAc)
      }

      def resultingQuantity(quantity: Int): AssetChangeBuilder = {
        val diff = quantity - assetChange.postEventPosition.signedQuantity
        val newAc =
          if (diff > 0) {
            assetChange.withPurchaseAmount(
              PurchaseAmount.fromAverages(diff, diff, diff / 100.0)
            )
          } else if (diff < 0) {
            assetChange.withSaleAmount(
              SaleAmount.fromAverages(-diff, -diff, -diff / 100.0)
            )
          } else {
            assetChange
          }
        assert(newAc.resultingPosition.signedQuantity == quantity)
        this.copy(assetChange = newAc)
      }

      private[AssetChangeTest] def done(): AssetChangesBuilder =
        seqBuilder.copy(assetChanges = seqBuilder.assetChanges :+ assetChange)
    }

    def assetChangesFor(asset: String = "asset", stockbroker: String = "stockbroker")(f: AssetChangesBuilder => AssetChangesBuilder): Seq[AssetChange] = {
      val initialBuilder = AssetChangesBuilder(asset, stockbroker, Vector.empty)
      val builder = f(initialBuilder)
      builder.assetChanges
    }

    implicit def singleBuilder2SeqBuilder(singleBuilder: AssetChangeBuilder): AssetChangesBuilder =
      singleBuilder.done()
  }
}
