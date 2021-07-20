package sisgrana
package investments.variableIncome.model

import investments.irpf.StockbrokerAsset
import investments.variableIncome.model.ctx._
import java.time.LocalDate

case class AssetChange(
  asset: String,
  stockbroker: String,
  date: LocalDate,

  previousPositionQuantity: Int,
  previousPositionAveragePrice: Double,
  previousPositionAverageCost: Double,

  eventTradeQuantity: Int,
  eventTradeTotalPurchaseValue: Double,
  eventTradeTotalPurchaseCost: Double,
  eventTradeTotalSaleValue: Double,
  eventTradeTotalSaleCost: Double,

  postEventPositionQuantity: Int,
  postEventPositionAveragePrice: Double,
  postEventPositionAverageCost: Double,

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

  val eventTradeResult: TradeResult =
    TradeResult.fromTotals(
      eventTradeQuantity,
      eventTradeTotalPurchaseValue,
      eventTradeTotalPurchaseCost,
      eventTradeTotalSaleValue,
      eventTradeTotalSaleCost,
    )

  val postEventPosition: AmountWithCost =
    AmountWithCost.fromSignedQuantityAndAverages(
      postEventPositionQuantity,
      postEventPositionAveragePrice,
      postEventPositionAverageCost,
    )

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

  lazy val postOperationsTradePosition: AmountWithCost =
    postEventPosition.withQuantity(postEventPosition.quantity - operationsTradeResult.quantity)

  lazy val nonTradeOperationsAmount: AmountWithCost =
    nonDayTradeOperationsAmount.withQuantity(nonDayTradeOperationsAmount.quantity - operationsTradeResult.quantity)

  lazy val swingTradeResult: TradeResult =
    eventTradeResult + operationsTradeResult

  def withPreviousPosition(previousPosition: AmountWithCost): AssetChange =
    this.copy(
      previousPositionQuantity = previousPosition.signedQuantity,
      previousPositionAveragePrice = previousPosition.averagePrice,
      previousPositionAverageCost = previousPosition.averageCost
    ).withSyncedResultingPositionFields()

  def withEventTradeResult(eventTradeResult: TradeResult): AssetChange =
    this.copy(
      eventTradeQuantity = eventTradeResult.quantity,
      eventTradeTotalPurchaseValue = eventTradeResult.totalPurchaseValue,
      eventTradeTotalPurchaseCost = eventTradeResult.totalPurchaseCost,
      eventTradeTotalSaleValue = eventTradeResult.totalSaleValue,
      eventTradeTotalSaleCost = eventTradeResult.totalSaleCost,
    ).withSyncedResultingPositionFields()

  def withPostEventPosition(postEventPosition: AmountWithCost): AssetChange =
    this.copy(
      postEventPositionQuantity = postEventPosition.signedQuantity,
      postEventPositionAveragePrice = postEventPosition.averagePrice,
      postEventPositionAverageCost = postEventPosition.averageCost,
    ).withSyncedResultingPositionFields()

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
      eventTradeQuantity = 0, eventTradeTotalPurchaseValue = 0.0, eventTradeTotalPurchaseCost = 0.0, eventTradeTotalSaleValue = 0.0, eventTradeTotalSaleCost = 0.0,
      postEventPositionQuantity = 0, postEventPositionAveragePrice = 0.0, postEventPositionAverageCost = 0.0,
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
