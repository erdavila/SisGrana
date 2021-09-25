package sisgrana
package investments.variableIncome.model

import investments.variableIncome.model.ctx._
import java.time.LocalDate
import scala.annotation.tailrec
import utils.{DateRange, DateRanges, oppositeSigns, sameNonZeroSigns}

// TODO: rename to AssetPeriod
// TODO: search for and rename everything that contains "change"
case class AssetChange(
  asset: String,
  stockbroker: String,

  date: LocalDate, // TODO: rename to beginDate
  endDate: LocalDate,

  previousPositionQuantity: Int,
  previousPositionAveragePrice: Double,
  previousPositionAverageCost: Double,

  eventEffectType: Option[String],

  eventSetPositionQuantity: Int,
  eventSetPositionAveragePrice: Double,
  eventSetPositionAverageCost: Double,
  eventConvertedToAsset: String, // TODO: remove
  eventConvertedToQuantity: Double, // TODO: remove

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
      case EventEffect.SetPosition.Type => EventEffect.SetPosition(eventSetPosition, eventConvertedToAsset, eventConvertedToQuantity)
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
      case Some(EventEffect.SetPosition(_, _, _)) => eventSetPosition
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

  def withPreviousPosition(previousPosition: Amount): AssetChange =
    this.copy(
      previousPositionQuantity = previousPosition.signedQuantity,
      previousPositionAveragePrice = previousPosition.averagePrice,
      previousPositionAverageCost = previousPosition.averageCost
    ).withSyncedResultingPositionFields()

  def withEventEffect(eventEffect: Option[EventEffect]): AssetChange =
    eventEffect match {
      case Some(EventEffect.SetPosition(pos, convToAsset, convToQty)) =>
        this.copy(eventEffectType = Some(EventEffect.SetPosition.Type))
          .withEventSetPositionAndNonSyncedResultingPosition(pos, convToAsset, convToQty)
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

  private def withEventSetPositionAndNonSyncedResultingPosition(
    eventSetPosition: Amount,
    eventConvertedToAsset: String = "",
    eventConvertedToQuantity: Double = 0.0,
  ): AssetChange =
    this.copy(
      eventSetPositionQuantity = eventSetPosition.signedQuantity,
      eventSetPositionAveragePrice = eventSetPosition.averagePrice,
      eventSetPositionAverageCost = eventSetPosition.averageCost,
      eventConvertedToAsset = eventConvertedToAsset,
      eventConvertedToQuantity = eventConvertedToQuantity,
    )

  private def withEventIncreaseAmountAndNonSyncedResultingPosition(eventIncreaseAmount: PurchaseAmount): AssetChange =
    this.copy(
      eventIncreaseQuantity = eventIncreaseAmount.quantity,
      eventIncreaseAveragePrice = eventIncreaseAmount.averagePrice,
      eventIncreaseAverageCost = eventIncreaseAmount.averageCost,
    )

  private def withEventDecreaseAmountAndNonSyncedResultingPosition(eventDecreaseAmount: SaleAmount): AssetChange =
    this.copy(
      eventDecreaseQuantity = eventDecreaseAmount.quantity,
      eventDecreaseAveragePrice = eventDecreaseAmount.averagePrice,
      eventDecreaseAverageCost = eventDecreaseAmount.averageCost,
    )

  def withPurchaseAmount(purchaseAmount: PurchaseAmount): AssetChange =
    this.copy(
      purchaseQuantity = purchaseAmount.quantity,
      purchaseAveragePrice = purchaseAmount.averagePrice,
      purchaseAverageCost = purchaseAmount.averageCost,
    ).withSyncedResultingPositionFields()

  def withSaleAmount(saleAmount: SaleAmount): AssetChange =
    this.copy(
      saleQuantity = saleAmount.quantity,
      saleAveragePrice = saleAmount.averagePrice,
      saleAverageCost = saleAmount.averageCost,
    ).withSyncedResultingPositionFields()

  def withExercisedQuantity(exercisedQuantity: Int): AssetChange =
    this.copy(
      exercisedQuantity = exercisedQuantity,
    ).withSyncedResultingPositionFields()

  private def withSyncedResultingPositionFields() =
    this.copy(
      resultingPositionQuantity = resultingPosition.signedQuantity,
      resultingPositionAveragePrice = resultingPosition.averagePrice,
      resultingPositionAverageCost = resultingPosition.averageCost,
    )

  def withConvertedTo(convertedTo: Option[ConvertedTo]): AssetChange =
    this.copy(
      convertedToAsset = convertedTo.map(_.asset),
      convertedToQuantity = convertedTo.map(_.quantity),
    )
}

object AssetChange extends LocalDateSupport {
  def withZeroes(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate): AssetChange =
    withZeroes(stockbrokerAsset, beginDate, LocalDateSupport.MaxDate)

  def withZeroes(stockbrokerAsset: StockbrokerAsset, beginDate: LocalDate, endDate: LocalDate): AssetChange =
    withZeroes(stockbrokerAsset.asset, stockbrokerAsset.stockbroker, beginDate, endDate)

  def withZeroes(asset: String, stockbroker: String, beginDate: LocalDate, endDate: LocalDate = LocalDateSupport.MaxDate): AssetChange =
    AssetChange(
      asset, stockbroker,
      beginDate, endDate,
      previousPositionQuantity = 0, previousPositionAveragePrice = 0.0, previousPositionAverageCost = 0.0,
      eventEffectType = None,
      eventSetPositionQuantity = 0, eventSetPositionAveragePrice = 0.0, eventSetPositionAverageCost = 0.0,
      eventConvertedToAsset = "", eventConvertedToQuantity = 0.0,
      eventIncreaseQuantity = 0, eventIncreaseAveragePrice = 0.0, eventIncreaseAverageCost = 0.0,
      eventDecreaseQuantity = 0, eventDecreaseAveragePrice = 0.0, eventDecreaseAverageCost = 0.0,
      purchaseQuantity = 0, purchaseAveragePrice = 0.0, purchaseAverageCost = 0.0,
      saleQuantity = 0, saleAveragePrice = 0.0, saleAverageCost = 0.0,
      exercisedQuantity = 0,
      resultingPositionQuantity = 0, resultingPositionAveragePrice = 0.0, resultingPositionAverageCost = 0.0,
      convertedToAsset = None, convertedToQuantity = None,
    )

  //noinspection TypeAnnotation
  // TODO: re-evaluate need for this method
  def latestAssetChangesAtDateQuery(maxDate: LocalDate) =
    ctx.quote {
      for {
        (asset, stockbroker, dateOpt) <- latestDateByAssetStockbroker(maxDate)
        ac <- query[AssetChange]
        if asset == ac.asset &&
          stockbroker == ac.stockbroker &&
          dateOpt.contains(ac.date)
      } yield ac
    }

  // TODO: re-evaluate need for this method
  private def latestDateByAssetStockbroker(maxDate: LocalDate) =
    ctx.quote {
      query[AssetChange]
        .filter(_.date <= lift(maxDate))
        .groupBy(ac => (ac.asset, ac.stockbroker))
        .map { case ((asset, stockbroker), changes) =>
          (asset, stockbroker, changes.map(_.date).max)
        }
    }

  // TODO: re-evaluate need for this method
  def toDateRanges(
    assetChanges: Seq[AssetChange],
    minDate: LocalDate,
    maxDate: LocalDate,
  )(implicit mode: DateRange.Mode): DateRanges = {
    for (Seq(acA, acB) <- assetChanges.sliding(2)) {
      require(acA.date `isBefore` acB.date)
    }
    require(assetChanges.map(_.stockbrokerAsset).distinct.lengthIs == 1)

    val interested = (ac: AssetChange) => ac.resultingPosition.quantity != 0
    val notInterested = (ac: AssetChange) => ! interested(ac)

    import utils.dateOrdering._

    @tailrec
    def loop(assetChanges: Seq[AssetChange], result: Vector[DateRange]): Seq[DateRange] = {
      assetChanges.dropWhile(notInterested) match {
        case acBegin +: rest if !(acBegin.date `isAfter` maxDate) =>
          lazy val beginDate = max(acBegin.date, minDate)
          rest.dropWhile(interested) match {
            case acEnd +: rest if !(acEnd.date `isBefore` minDate) =>
              val endDate = min(acEnd.date, maxDate)
              loop(rest, result :+ DateRange(beginDate, endDate))
            case _ +: rest => loop(rest, result)
            case _ => result :+ DateRange(beginDate, maxDate)
          }
        case _ +: _ => result
        case _ => result
      }
    }

    val result = loop(assetChanges, Vector.empty)
    DateRanges.from(result)
  }
}
