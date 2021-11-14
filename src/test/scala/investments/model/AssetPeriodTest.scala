package sisgrana
package investments.model

import TestBase.{DefaultAsset, DefaultStockbroker}
import investments.commands.multiImport.eventsAndBrokerageNotes.EventOutcome
import investments.model.AssetPeriodTest.DSL._
import java.time.LocalDate
import org.scalatest.Inside.inside
import scala.language.implicitConversions
import utils.DateRangeTest.int2Date
import utils.sameNonZeroSigns

class AssetPeriodTest extends TestBase {
  private val ZeroAssetPeriod = AssetPeriod.withZeroes("asset", "stockbroker", LocalDate.now())

  test("previousPosition, withPreviousPosition(), and fields") {
    val cases = Table(
      "previousPosition",
      PurchaseAmount.Zero,
      PurchaseAmount.fromAverages(10, 20.00, 0.10),
      SaleAmount.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { previousPosition =>
      val assetPeriod = ZeroAssetPeriod.withPreviousPosition(previousPosition)

      assetPeriod.previousPosition should equal (previousPosition)
      assetPeriod.previousPositionQuantity should equal (previousPosition.signedQuantity)
      assetPeriod.previousPositionAveragePrice should equal (previousPosition.averagePrice)
      assetPeriod.previousPositionAverageCost should equal (previousPosition.averageCost)
    }
  }

  test("None: eventEffect, withEventEffect(), and fields") {
    val assetPeriod = ZeroAssetPeriod.withEventEffect(None)

    assetPeriod.eventEffect shouldBe empty
    assetPeriod.eventSetPosition should equal (PurchaseAmount.Zero)
    assetPeriod.eventIncreaseAmount should equal (PurchaseAmount.Zero)
    assetPeriod.eventDecreaseAmount should equal (SaleAmount.Zero)
    assetPeriod.eventEffectType shouldBe empty
    assetPeriod.eventSetPositionQuantity should equal (0)
    assetPeriod.eventSetPositionAveragePrice should equal (0.0)
    assetPeriod.eventSetPositionAverageCost should equal (0.0)
    assetPeriod.eventIncreaseQuantity should equal (0)
    assetPeriod.eventIncreaseAveragePrice should equal (0.0)
    assetPeriod.eventIncreaseAverageCost should equal (0.0)
    assetPeriod.eventDecreaseQuantity should equal (0)
    assetPeriod.eventDecreaseAveragePrice should equal (0.0)
    assetPeriod.eventDecreaseAverageCost should equal (0.0)
  }

  test("eventSetPosition, eventEffect, eventConvertedTo*, withEventEffect(), and fields") {
    val cases = Table(
      "eventEffect",
      EventEffect.SetPosition(PurchaseAmount.Zero),
      EventEffect.SetPosition(PurchaseAmount.fromAverages(10, 10.00, 0.10)),
      EventEffect.SetPosition(SaleAmount.fromAverages(10, 10.00, 0.10)),
    )

    forAll(cases) { eventEffect =>
      val assetPeriod = ZeroAssetPeriod.withEventEffect(Some(eventEffect))

      assetPeriod.eventEffect should contain (eventEffect)
      assetPeriod.eventSetPosition should equal (eventEffect.position)
      assetPeriod.eventEffectType should contain (EventEffect.SetPosition.Type)
      assetPeriod.eventSetPositionQuantity should equal (eventEffect.position.signedQuantity)
      assetPeriod.eventSetPositionAveragePrice should equal (eventEffect.position.averagePrice)
      assetPeriod.eventSetPositionAverageCost should equal (eventEffect.position.averageCost)
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
      val assetPeriod = ZeroAssetPeriod.withEventEffect(Some(eventEffect))

      assetPeriod.eventEffect should contain (eventEffect)
      assetPeriod.eventIncreaseAmount should equal (eventEffect.increaseAmount)
      assetPeriod.eventDecreaseAmount should equal (eventEffect.decreaseAmount)
      assetPeriod.eventEffectType should contain (EventEffect.AddToPosition.Type)
      assetPeriod.eventIncreaseQuantity should equal (eventEffect.increaseAmount.quantity)
      assetPeriod.eventIncreaseAveragePrice should equal (eventEffect.increaseAmount.averagePrice)
      assetPeriod.eventIncreaseAverageCost should equal (eventEffect.increaseAmount.averageCost)
      assetPeriod.eventDecreaseQuantity should equal (eventEffect.decreaseAmount.quantity)
      assetPeriod.eventDecreaseAveragePrice should equal (eventEffect.decreaseAmount.averagePrice)
      assetPeriod.eventDecreaseAverageCost should equal (eventEffect.decreaseAmount.averageCost)
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
      val assetPeriod = ZeroAssetPeriod
        .withPreviousPosition(previousPosition)
        .withEventEffect(Some(EventEffect.AddToPosition(eventIncreaseAmount, eventDecreaseAmount)))

      assetPeriod.eventTradeResult should equal (expectedEventTradeResult)
      assetPeriod.nonTradeEventPosition should equal (expectedNonTradeEventPosition)
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
      val assetPeriod = ZeroAssetPeriod
        .withPreviousPosition(previousPosition)
        .withEventEffect(eventEffect)

      assetPeriod.postEventPosition should equal (expectedPostEventPosition)
    }
  }

  test("purchaseAmount, withPurchaseAmount(), and fields") {
    val cases = Table(
      "purchaseAmount",
      PurchaseAmount.Zero,
      PurchaseAmount.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { purchaseAmount =>
      val assetPeriod = ZeroAssetPeriod.withPurchaseAmount(purchaseAmount)

      assetPeriod.purchaseAmount should equal (purchaseAmount)
      assetPeriod.purchaseQuantity should equal (purchaseAmount.quantity)
      assetPeriod.purchaseAveragePrice should equal (purchaseAmount.averagePrice)
      assetPeriod.purchaseAverageCost should equal (purchaseAmount.averageCost)
    }
  }

  test("saleAmount, withSaleAmount(), and fields") {
    val cases = Table(
      "saleAmount",
      SaleAmount.Zero,
      SaleAmount.fromAverages(10, 20.00, 0.10),
    )

    forAll(cases) { saleAmount =>
      val assetPeriod = ZeroAssetPeriod.withSaleAmount(saleAmount)

      assetPeriod.saleAmount should equal (saleAmount)
      assetPeriod.saleQuantity should equal (saleAmount.quantity)
      assetPeriod.saleAveragePrice should equal (saleAmount.averagePrice)
      assetPeriod.saleAverageCost should equal (saleAmount.averageCost)
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
      val assetPeriod = ZeroAssetPeriod
        .withPurchaseAmount(purchaseAmount)
        .withSaleAmount(saleAmount)

      assetPeriod.dayTradeResult should equal (expectedDayTradeResult)
      assetPeriod.nonDayTradeOperationsAmount should equal (expectedNonDayTradeOperationsAmount)
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
      val ap0 = ZeroAssetPeriod.withEventEffect(Some(EventEffect.SetPosition(postEventPosition)))
      val assetPeriod =
        (nonDayTradeOperationsAmount: Amount) match {
          case p@PurchaseAmount(_, _, _) => ap0.withPurchaseAmount(p)
          case s@SaleAmount(_, _, _) => ap0.withSaleAmount(s)
        }

      assetPeriod.resultingPosition should equal (expectedResultingPosition)
      assetPeriod.operationsTradeResult should equal (expectedOperationsTradeResult)
    }
  }

  test("convertedTo, withConvertedTo(), and fields") {
    val cases = Table(
      "convertedTo option",
      Some(ConvertedTo("Y", 10.0)),
      None
    )

    val NonZeroAssetPeriod = ZeroAssetPeriod
      .withPurchaseAmount(PurchaseAmount.fromAverages(5, 1.00, 0.01))

    forAll(cases) { convertedToOpt =>
      val assetPeriod = NonZeroAssetPeriod.withConvertedTo(convertedToOpt)

      assetPeriod.convertedTo should equal (convertedToOpt)
      assetPeriod.convertedToAsset.isDefined should equal (convertedToOpt.isDefined)
      assetPeriod.convertedToQuantity.isDefined should equal (convertedToOpt.isDefined)
      assetPeriod.convertedToAsset should equal (convertedToOpt.map(_.asset))
      assetPeriod.convertedToQuantity should equal (convertedToOpt.map(_.quantity))
    }
  }

  //noinspection ZeroIndexToHead
  test("AssetPeriodsBuilder") {
    {
      val result = assetPeriodsFor()(_
        .on(1).resultingQuantity(3)
        .on(3).resultingQuantity(7)
      )

      result should have length 2

      result(0).beginDate should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).previousPosition.quantity should equal (0)
      result(0).purchaseAmount.quantity should equal (3)
      result(0).resultingPosition.quantity should equal (3)

      result(1).beginDate should equal (int2Date(3))
      result(1).previousPosition.quantity should equal (3)
      result(1).purchaseAmount.quantity should equal (4)
      result(1).resultingPosition.quantity should equal (7)
    }

    {
      val result = assetPeriodsFor("a1")(_
        .on(1).resultingQuantity(7)
        .on(3).eventConvertedToSame(14)
      )

      result should have length 2

      result(0).beginDate should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (7)
      result(0).convertedTo should contain (ConvertedTo("a1", 14.0))

      result(1).beginDate should equal (int2Date(3))
      inside(result(1).eventEffect) { case Some(sp@ EventEffect.SetPosition(_)) =>
        sp.position.quantity should equal (14)
      }
      result(1).resultingPosition.quantity should equal (14)
      result(1).convertedTo should be (empty)
    }

    {
      val result = assetPeriodsFor("a1")(_
        .on(1).resultingQuantity(7)
        .on(3).eventConvertedToOther("a2")
      )

      result should have length 2

      result(0).beginDate should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (7)
      result(0).convertedTo should contain (ConvertedTo("a2", 7.0))

      result(1).beginDate should equal (int2Date(3))
      result(1).previousPosition.quantity should equal (7)
      inside(result(1).eventEffect) { case Some(sp@ EventEffect.SetPosition(_)) =>
        sp.position.quantity should equal (0)
      }
      result(1).resultingPosition.quantity should equal (0)
      result(1).convertedTo should be (empty)
    }

    {
      val result = assetPeriodsFor()(_
        .on(1).resultingQuantity(3)
        .on(3).eventIncreasedQuantity(4)
        .on(5).eventIncreasedQuantity(2).resultingQuantity(10)
      )

      result should have length 3

      result(0).beginDate should equal (int2Date(1))
      result(0).endDate should equal (int2Date(3))
      result(0).resultingPosition.quantity should equal (3)

      result(1).beginDate should equal (int2Date(3))
      result(1).endDate should equal (int2Date(5))
      result(1).previousPosition.quantity should equal (3)
      inside(result(1).eventEffect) { case Some(atp@ EventEffect.AddToPosition(_, _)) =>
        atp.increaseAmount.quantity should equal (4)
      }
      result(1).resultingPosition.quantity should equal (7)

      result(2).beginDate should equal (int2Date(5))
      result(2).previousPosition.quantity should equal (7)
      inside(result(2).eventEffect) { case Some(atp@ EventEffect.AddToPosition(_, _)) =>
        atp.increaseAmount.quantity should equal (2)
      }
      result(2).purchaseAmount.quantity should equal (1)
      result(2).resultingPosition.quantity should equal (10)
    }
  }
}

object AssetPeriodTest {

  object DSL {
    val NonZero = 999

    case class AssetPeriodsBuilder(asset: String, stockbroker: String, assetPeriods: Vector[AssetPeriod]) {
      def on(date: LocalDate): AssetPeriodBuilder = {
        val (previousPosition, seqBuilder) = assetPeriods match {
          case init :+ last => (last.resultingPosition, this.copy(assetPeriods = init :+ last.copy(endDate = date)))
          case _ => (Amount.Zero, this)
        }

        val ap = AssetPeriod
          .withZeroes(asset, stockbroker, date)
          .withPreviousPosition(previousPosition)

        AssetPeriodBuilder(seqBuilder, ap)
      }
    }

    case class AssetPeriodBuilder(seqBuilder: AssetPeriodsBuilder, assetPeriod: AssetPeriod) {
      def eventConvertedToSame(quantity: Int): AssetPeriodBuilder = {
        require(sameNonZeroSigns(assetPeriod.previousPosition.signedQuantity, quantity))
        require(seqBuilder.assetPeriods.nonEmpty)

        val amount = Amount.fromSignedQuantityAndAverages(quantity, 1.00, 0.01)
        eventConvertedTo(amount, assetPeriod.asset, quantity)
      }

      def eventConvertedToOther(otherAsset: String): AssetPeriodBuilder = {
        require(otherAsset != seqBuilder.asset)

        val quantity = assetPeriod.previousPosition.signedQuantity
        eventConvertedTo(Amount.Zero, otherAsset, quantity)
      }

      private def eventConvertedTo(amount: Amount, asset: String, quantity: Double): AssetPeriodBuilder = {
        require(seqBuilder.assetPeriods.nonEmpty)

        val previousAPs = seqBuilder.assetPeriods
        val newPreviousAPs = previousAPs.init :+ previousAPs.last.withConvertedTo(Some(ConvertedTo(asset, quantity)))

        this.copy(
          seqBuilder = seqBuilder.copy(assetPeriods = newPreviousAPs),
          assetPeriod = assetPeriod.withEventEffect(Some(EventEffect.SetPosition(amount))),
        )
      }

      def eventIncreasedQuantity(quantity: Int): AssetPeriodBuilder = {
        require(quantity != 0)
        val amount = Amount.fromSignedQuantityAndAverages(quantity, 1.00, 0.01)
        val newAP = assetPeriod.withEventEffect(Some(EventOutcome.AddToPosition(amount).toEffect))
        this.copy(assetPeriod = newAP)
      }

      def resultingQuantity(quantity: Int): AssetPeriodBuilder = {
        val diff = quantity - assetPeriod.postEventPosition.signedQuantity
        val newAP =
          if (diff > 0) {
            assetPeriod.withPurchaseAmount(
              PurchaseAmount.fromAverages(diff, diff, diff / 100.0)
            )
          } else if (diff < 0) {
            assetPeriod.withSaleAmount(
              SaleAmount.fromAverages(-diff, -diff, -diff / 100.0)
            )
          } else {
            assetPeriod
          }
        assert(newAP.resultingPosition.signedQuantity == quantity)
        this.copy(assetPeriod = newAP)
      }

      private[AssetPeriodTest] def done(): AssetPeriodsBuilder =
        seqBuilder.copy(assetPeriods = seqBuilder.assetPeriods :+ assetPeriod)
    }

    def assetPeriodsFor(asset: String = DefaultAsset, stockbroker: String = DefaultStockbroker)(f: AssetPeriodsBuilder => AssetPeriodsBuilder): Seq[AssetPeriod] = {
      val initialBuilder = AssetPeriodsBuilder(asset, stockbroker, Vector.empty)
      val builder = f(initialBuilder)
      builder.assetPeriods
    }

    implicit def singleBuilder2SeqBuilder(singleBuilder: AssetPeriodBuilder): AssetPeriodsBuilder =
      singleBuilder.done()
  }
}
