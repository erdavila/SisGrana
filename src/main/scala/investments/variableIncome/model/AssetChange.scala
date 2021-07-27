package sisgrana
package investments.variableIncome.model

import investments.variableIncome.model.ctx._
import java.time.LocalDate

case class AssetChange(
  asset: String,
  stockbroker: String,
  date: LocalDate,

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

  resultingPositionQuantity: Int,
  resultingPositionAveragePrice: Double,
  resultingPositionAverageCost: Double,
) {
  // Requirements are delegated to the other case classes requirements

  lazy val stockbrokerAsset: StockbrokerAsset =
    StockbrokerAsset(stockbroker, asset)

  val previousPosition: AmountWithCost =
    AmountWithCost.fromSignedQuantityAndAverages(
      previousPositionQuantity,
      previousPositionAveragePrice,
      previousPositionAverageCost,
    )

  val eventSetPosition: AmountWithCost = {
    assert(eventEffectType.contains(EventEffect.SetPosition.Type) || eventSetPositionQuantity == 0)
    AmountWithCost.fromSignedQuantityAndAverages(
      eventSetPositionQuantity,
      eventSetPositionAveragePrice,
      eventSetPositionAverageCost,
    )
  }

  val eventIncreaseAmount: PurchaseAmountWithCost = {
    assert(eventEffectType.contains(EventEffect.AddToPosition.Type) || eventIncreaseQuantity == 0)
    PurchaseAmountWithCost.fromAverages(
      eventIncreaseQuantity,
      eventIncreaseAveragePrice,
      eventIncreaseAverageCost,
    )
  }

  val eventDecreaseAmount: SaleAmountWithCost = {
    assert(eventEffectType.contains(EventEffect.AddToPosition.Type) || eventDecreaseQuantity == 0)
    SaleAmountWithCost.fromAverages(
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

  lazy val (eventTradeResult: TradeResult, nonTradeEventPosition: AmountWithCost) =
    previousPosition match {
      case p@PurchaseAmountWithCost(_, _, _) => TradeResult.from(p + eventIncreaseAmount, eventDecreaseAmount)
      case s@SaleAmountWithCost(_, _, _) => TradeResult.from(eventIncreaseAmount, s + eventDecreaseAmount)
    }

  lazy val postEventPosition: AmountWithCost =
    eventEffect match {
      case Some(EventEffect.SetPosition(_)) => eventSetPosition
      case Some(EventEffect.AddToPosition(_, _)) => nonTradeEventPosition
      case None => previousPosition
    }

  val purchaseAmount: PurchaseAmountWithCost =
    PurchaseAmountWithCost.fromAverages(
      purchaseQuantity,
      purchaseAveragePrice,
      purchaseAverageCost,
    )

  val saleAmount: SaleAmountWithCost =
    SaleAmountWithCost.fromAverages(
      saleQuantity,
      saleAveragePrice,
      saleAverageCost,
    )

  lazy val (dayTradeResult: TradeResult, nonDayTradeOperationsAmount: AmountWithCost) =
    TradeResult.from(purchaseAmount, saleAmount)

  lazy val (resultingPosition: AmountWithCost, operationsTradeResult: TradeResult) =
    AmountWithCost.combine(postEventPosition, nonDayTradeOperationsAmount)

  lazy val swingTradeResult: TradeResult =
    eventTradeResult + operationsTradeResult

  def withPreviousPosition(previousPosition: AmountWithCost): AssetChange =
    this.copy(
      previousPositionQuantity = previousPosition.signedQuantity,
      previousPositionAveragePrice = previousPosition.averagePrice,
      previousPositionAverageCost = previousPosition.averageCost
    ).withSyncedResultingPositionFields()

  def withEventEffect(eventEffect: Option[EventEffect]): AssetChange =
    eventEffect match {
      case Some(EventEffect.SetPosition(pos)) =>
        this.copy(eventEffectType = Some(EventEffect.SetPosition.Type))
          .withEventSetPositionAndNonSyncedResultingPosition(pos)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(PurchaseAmountWithCost.Zero)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(SaleAmountWithCost.Zero)
          .withSyncedResultingPositionFields()
      case Some(EventEffect.AddToPosition(inc, dec)) =>
        this.copy(eventEffectType = Some(EventEffect.AddToPosition.Type))
          .withEventSetPositionAndNonSyncedResultingPosition(AmountWithCost.Zero)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(inc)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(dec)
          .withSyncedResultingPositionFields()
      case None =>
        this.copy(eventEffectType = None)
          .withEventSetPositionAndNonSyncedResultingPosition(AmountWithCost.Zero)
          .withEventIncreaseAmountAndNonSyncedResultingPosition(PurchaseAmountWithCost.Zero)
          .withEventDecreaseAmountAndNonSyncedResultingPosition(SaleAmountWithCost.Zero)
          .withSyncedResultingPositionFields()
    }

  private def withEventSetPositionAndNonSyncedResultingPosition(eventSetPosition: AmountWithCost): AssetChange =
    this.copy(
      eventSetPositionQuantity = eventSetPosition.signedQuantity,
      eventSetPositionAveragePrice = eventSetPosition.averagePrice,
      eventSetPositionAverageCost = eventSetPosition.averageCost,
    )

  private def withEventIncreaseAmountAndNonSyncedResultingPosition(eventIncreaseAmount: PurchaseAmountWithCost): AssetChange =
    this.copy(
      eventIncreaseQuantity = eventIncreaseAmount.quantity,
      eventIncreaseAveragePrice = eventIncreaseAmount.averagePrice,
      eventIncreaseAverageCost = eventIncreaseAmount.averageCost,
    )

  private def withEventDecreaseAmountAndNonSyncedResultingPosition(eventDecreaseAmount: SaleAmountWithCost): AssetChange =
    this.copy(
      eventDecreaseQuantity = eventDecreaseAmount.quantity,
      eventDecreaseAveragePrice = eventDecreaseAmount.averagePrice,
      eventDecreaseAverageCost = eventDecreaseAmount.averageCost,
    )

  def withPurchaseAmount(purchaseAmount: PurchaseAmountWithCost): AssetChange =
    this.copy(
      purchaseQuantity = purchaseAmount.quantity,
      purchaseAveragePrice = purchaseAmount.averagePrice,
      purchaseAverageCost = purchaseAmount.averageCost,
    ).withSyncedResultingPositionFields()

  def withSaleAmount(saleAmount: SaleAmountWithCost): AssetChange =
    this.copy(
      saleQuantity = saleAmount.quantity,
      saleAveragePrice = saleAmount.averagePrice,
      saleAverageCost = saleAmount.averageCost,
    ).withSyncedResultingPositionFields()

  private def withSyncedResultingPositionFields() =
    this.copy(
      resultingPositionQuantity = resultingPosition.signedQuantity,
      resultingPositionAveragePrice = resultingPosition.averagePrice,
      resultingPositionAverageCost = resultingPosition.averageCost,
    )
}

object AssetChange extends LocalDateSupport {
  def withZeroes(stockbrokerAsset: StockbrokerAsset, date: LocalDate): AssetChange =
    withZeroes(stockbrokerAsset.asset, stockbrokerAsset.stockbroker, date)

  def withZeroes(asset: String, stockbroker: String, date: LocalDate): AssetChange =
    AssetChange(
      asset, stockbroker, date,
      previousPositionQuantity = 0, previousPositionAveragePrice = 0.0, previousPositionAverageCost = 0.0,
      eventEffectType = None,
      eventSetPositionQuantity = 0, eventSetPositionAveragePrice = 0.0, eventSetPositionAverageCost = 0.0,
      eventIncreaseQuantity = 0, eventIncreaseAveragePrice = 0.0, eventIncreaseAverageCost = 0.0,
      eventDecreaseQuantity = 0, eventDecreaseAveragePrice = 0.0, eventDecreaseAverageCost = 0.0,
      purchaseQuantity = 0, purchaseAveragePrice = 0.0, purchaseAverageCost = 0.0,
      saleQuantity = 0, saleAveragePrice = 0.0, saleAverageCost = 0.0,
      resultingPositionQuantity = 0, resultingPositionAveragePrice = 0.0, resultingPositionAverageCost = 0.0,
    )

  //noinspection TypeAnnotation
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

  private def latestDateByAssetStockbroker(maxDate: LocalDate) =
    ctx.quote {
      query[AssetChange]
        .filter(_.date <= lift(maxDate))
        .groupBy(ac => (ac.asset, ac.stockbroker))
        .map { case ((asset, stockbroker), changes) =>
          (asset, stockbroker, changes.map(_.date).max)
        }
    }
}
