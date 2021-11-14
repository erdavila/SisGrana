package sisgrana
package investments.model

import investments.model.ctx._
import java.time.LocalDate
import utils.{DateRange, oppositeSigns, sameNonZeroSigns}

case class AssetPeriod(
  asset: String,
  stockbroker: String,

  beginDate: LocalDate,
  endDate: LocalDate,

  previousPositionQuantity: Int,
  previousPositionAveragePrice: Double,
  previousPositionAverageCost: Double,

  eventEffectType: Option[String],

  eventSetPositionQuantity: Int,
  eventSetPositionAveragePrice: Double,
  eventSetPositionAverageCost: Double,

  eventIncreaseQuantity: Int,
  eventIncreaseAveragePrice: Double,
  eventIncreaseAverageCost: Double,

  eventDecreaseQuantity: Int,
  eventDecreaseAveragePrice: Double,
  eventDecreaseAverageCost: Double,

  purchaseQuantity: Int,
  purchaseAveragePrice: Double,
  purchaseAverageCost: Double,

  saleQuantity: Int,
  saleAveragePrice: Double,
  saleAverageCost: Double,

  exercisedQuantity: Int,

  resultingPositionQuantity: Int,
  resultingPositionAveragePrice: Double,
  resultingPositionAverageCost: Double,

  convertedToAsset: Option[String],
  convertedToQuantity: Option[Double],
) {
  // Requirements are delegated to the other case classes requirements

  lazy val stockbrokerAsset: StockbrokerAsset =
    StockbrokerAsset(stockbroker, asset)

  lazy val dateRange: DateRange =
    DateRange(beginDate, endDate)

  val previousPosition: Amount =
    Amount.fromSignedQuantityAndAverages(
      previousPositionQuantity,
      previousPositionAveragePrice,
      previousPositionAverageCost,
    )

  val eventSetPosition: Amount = {
    assert(eventEffectType.contains(EventEffect.SetPosition.Type) || eventSetPositionQuantity == 0)
    Amount.fromSignedQuantityAndAverages(
      eventSetPositionQuantity,
      eventSetPositionAveragePrice,
      eventSetPositionAverageCost,
    )
  }

  val eventIncreaseAmount: PurchaseAmount = {
    assert(eventEffectType.contains(EventEffect.AddToPosition.Type) || eventIncreaseQuantity == 0)
    PurchaseAmount.fromAverages(
      eventIncreaseQuantity,
      eventIncreaseAveragePrice,
      eventIncreaseAverageCost,
    )
  }

  val eventDecreaseAmount: SaleAmount = {
    assert(eventEffectType.contains(EventEffect.AddToPosition.Type) || eventDecreaseQuantity == 0)
    SaleAmount.fromAverages(
      eventDecreaseQuantity,
      eventDecreaseAveragePrice,
      eventDecreaseAverageCost,
    )
  }

  lazy val eventEffect: Option[EventEffect] =
    eventEffectType.map {
      case EventEffect.SetPosition.Type => EventEffect.SetPosition(eventSetPosition)
      case EventEffect.AddToPosition.Type => EventEffect.AddToPosition(eventIncreaseAmount, eventDecreaseAmount)
      case t => throw new Exception(s"Unknown type: $t")
    }

  lazy val (eventTradeResult: TradeResult, nonTradeEventPosition: Amount) =
    previousPosition match {
      case p@PurchaseAmount(_, _, _) => TradeResult.from(p + eventIncreaseAmount, eventDecreaseAmount)
      case s@SaleAmount(_, _, _) => TradeResult.from(eventIncreaseAmount, s + eventDecreaseAmount)
    }

  lazy val postEventPosition: Amount =
    eventEffect match {
      case Some(EventEffect.SetPosition(_)) => eventSetPosition
      case Some(EventEffect.AddToPosition(_, _)) => nonTradeEventPosition
      case None => previousPosition
    }

  val purchaseAmount: PurchaseAmount =
    PurchaseAmount.fromAverages(
      purchaseQuantity,
      purchaseAveragePrice,
      purchaseAverageCost,
    )

  val saleAmount: SaleAmount =
    SaleAmount.fromAverages(
      saleQuantity,
      saleAveragePrice,
      saleAverageCost,
    )

  lazy val (dayTradeResult: TradeResult, nonDayTradeOperationsAmount: Amount) =
    TradeResult.from(purchaseAmount, saleAmount)

  lazy val (resultingPosition: Amount, operationsTradeResult: TradeResult) = {
    val postExerciseQuantity = {
      require(!oppositeSigns(postEventPosition.signedQuantity, exercisedQuantity))
      val quantity = postEventPosition.signedQuantity - exercisedQuantity
      if (quantity == 0) {
        Amount.Zero
      } else {
        Amount.fromSignedQuantityAndAverages(
          quantity,
          postEventPosition.averagePrice,
          postEventPosition.averageCost,
        )
      }
    }

    Amount.combine(postExerciseQuantity, nonDayTradeOperationsAmount)
  }

  lazy val swingTradeResult: TradeResult =
    eventTradeResult + operationsTradeResult

  val convertedTo: Option[ConvertedTo] = {
    require(convertedToAsset.isDefined == convertedToQuantity.isDefined)
    for {
      asset <- convertedToAsset
      quantity <- convertedToQuantity
      _ = assert(sameNonZeroSigns(resultingPositionQuantity, quantity))
    } yield ConvertedTo(asset, quantity)
  }

  def withPreviousPosition(previousPosition: Amount): AssetPeriod =
    this.copy(
      previousPositionQuantity = previousPosition.signedQuantity,
      previousPositionAveragePrice = previousPosition.averagePrice,
      previousPositionAverageCost = previousPosition.averageCost
    ).withSyncedResultingPositionFields()

  def withEventEffect(eventEffect: Option[EventEffect]): AssetPeriod =
    eventEffect match {
      case Some(EventEffect.SetPosition(pos)) =>
        this.copy(eventEffectType = Some(EventEffect.SetPosition.Type))
          .withEventSetPositionAndNonSyncedResultingPosition(pos)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(PurchaseAmount.Zero)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(SaleAmount.Zero)
          .withSyncedResultingPositionFields()
      case Some(EventEffect.AddToPosition(inc, dec)) =>
        this.copy(eventEffectType = Some(EventEffect.AddToPosition.Type))
          .withEventSetPositionAndNonSyncedResultingPosition(Amount.Zero)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(inc)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(dec)
          .withSyncedResultingPositionFields()
      case None =>
        this.copy(eventEffectType = None)
          .withEventSetPositionAndNonSyncedResultingPosition(Amount.Zero)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(PurchaseAmount.Zero)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(SaleAmount.Zero)
          .withSyncedResultingPositionFields()
    }

  private def withEventSetPositionAndNonSyncedResultingPosition(eventSetPosition: Amount): AssetPeriod =
    this.copy(
      eventSetPositionQuantity = eventSetPosition.signedQuantity,
      eventSetPositionAveragePrice = eventSetPosition.averagePrice,
      eventSetPositionAverageCost = eventSetPosition.averageCost,
    )

  private def withEventIncreaseAmountAndNonSyncedResultingPosition(eventIncreaseAmount: PurchaseAmount): AssetPeriod =
    this.copy(
      eventIncreaseQuantity = eventIncreaseAmount.quantity,
      eventIncreaseAveragePrice = eventIncreaseAmount.averagePrice,
      eventIncreaseAverageCost = eventIncreaseAmount.averageCost,
    )

  private def withEventDecreaseAmountAndNonSyncedResultingPosition(eventDecreaseAmount: SaleAmount): AssetPeriod =
    this.copy(
      eventDecreaseQuantity = eventDecreaseAmount.quantity,
      eventDecreaseAveragePrice = eventDecreaseAmount.averagePrice,
      eventDecreaseAverageCost = eventDecreaseAmount.averageCost,
    )

  def withPurchaseAmount(purchaseAmount: PurchaseAmount): AssetPeriod =
    this.copy(
      purchaseQuantity = purchaseAmount.quantity,
      purchaseAveragePrice = purchaseAmount.averagePrice,
      purchaseAverageCost = purchaseAmount.averageCost,
    ).withSyncedResultingPositionFields()

  def withSaleAmount(saleAmount: SaleAmount): AssetPeriod =
    this.copy(
      saleQuantity = saleAmount.quantity,
      saleAveragePrice = saleAmount.averagePrice,
      saleAverageCost = saleAmount.averageCost,
    ).withSyncedResultingPositionFields()

  def withExercisedQuantity(exercisedQuantity: Int): AssetPeriod =
    this.copy(
      exercisedQuantity = exercisedQuantity,
    ).withSyncedResultingPositionFields()

  private def withSyncedResultingPositionFields() =
    this.copy(
      resultingPositionQuantity = resultingPosition.signedQuantity,
      resultingPositionAveragePrice = resultingPosition.averagePrice,
      resultingPositionAverageCost = resultingPosition.averageCost,
    )

  def withConvertedTo(convertedTo: Option[ConvertedTo]): AssetPeriod =
    this.copy(
      convertedToAsset = convertedTo.map(_.asset),
      convertedToQuantity = convertedTo.map(_.quantity),
    )
}

object AssetPeriod extends LocalDateSupport {
  def withZeroes(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate): AssetPeriod =
    withZeroes(stockbrokerAsset, beginDate, LocalDateSupport.MaxDate)

  def withZeroes(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDate: LocalDate): AssetPeriod =
    withZeroes(stockbrokerAsset.asset, stockbrokerAsset.stockbroker, beginDate, endDate)

  def withZeroes(asset: String, stockbroker: String, beginDate: LocalDate, endDate: LocalDate = LocalDateSupport.MaxDate): AssetPeriod =
    AssetPeriod(
      asset, stockbroker,
      beginDate, endDate,
      previousPositionQuantity = 0, previousPositionAveragePrice = 0.0, previousPositionAverageCost = 0.0,
      eventEffectType = None,
      eventSetPositionQuantity = 0, eventSetPositionAveragePrice = 0.0, eventSetPositionAverageCost = 0.0,
      eventIncreaseQuantity = 0, eventIncreaseAveragePrice = 0.0, eventIncreaseAverageCost = 0.0,
      eventDecreaseQuantity = 0, eventDecreaseAveragePrice = 0.0, eventDecreaseAverageCost = 0.0,
      purchaseQuantity = 0, purchaseAveragePrice = 0.0, purchaseAverageCost = 0.0,
      saleQuantity = 0, saleAveragePrice = 0.0, saleAverageCost = 0.0,
      exercisedQuantity = 0,
      resultingPositionQuantity = 0, resultingPositionAveragePrice = 0.0, resultingPositionAverageCost = 0.0,
      convertedToAsset = None, convertedToQuantity = None,
    )

  //noinspection TypeAnnotation
  def inDateRangeQuery(dateRange: DateRange) =
    betweenDatesQuery(dateRange.beginDate, dateRange.endDate)

  //noinspection TypeAnnotation
  def betweenDatesQuery(minDate: LocalDate, maxDate: LocalDate) =
    ctx.quote(
      query[AssetPeriod]
        .filter(_.beginDate <= lift(maxDate))
        .filter(_.endDate >= lift(minDate))
    )
}
