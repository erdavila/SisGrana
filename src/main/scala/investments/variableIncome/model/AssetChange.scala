package sisgrana
package investments.variableIncome.model

import investments.irpf.StockbrokerAsset
import investments.variableIncome.model.ctx._
import java.time.LocalDate

case class AssetChange(
  asset: String,
  stockbroker: String,
  date: LocalDate,

  byEvent: Boolean,

  previousPositionQuantity: Int,
  previousPositionAveragePrice: Double,
  previousPositionAverageCost: Double,

  eventTradeResultQuantity: Int,
  eventTradeResultTotalGrossValue: Double,
  eventTradeResultTotalCost: Double,

  postEventPositionQuantity: Int,
  postEventPositionAveragePrice: Double,
  postEventPositionAverageCost: Double,

  purchaseQuantity: Int,
  purchaseAveragePrice: Double,
  purchaseAverageCost: Double,

  saleQuantity: Int,
  saleAveragePrice: Double,
  saleAverageCost: Double,

  dayTradeResultQuantity: Int,
  dayTradeResultTotalGrossValue: Double,
  dayTradeResultTotalCost: Double,

  nonDayTradeOperationsQuantity: Int,
  nonDayTradeOperationsAveragePrice: Double,
  nonDayTradeOperationsAverageCost: Double,

  operationsTradeResultQuantity: Int,
  operationsTradeResultTotalGrossValue: Double,
  operationsTradeResultTotalCost: Double,

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
      eventTradeResultQuantity,
      eventTradeResultTotalGrossValue,
      eventTradeResultTotalCost,
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

  val dayTradeResult: TradeResult =
    TradeResult.fromTotals(dayTradeResultQuantity, dayTradeResultTotalGrossValue, dayTradeResultTotalCost)

  val nonDayTradeOperationsAmount: AmountWithCost =
    AmountWithCost.fromSignedQuantityAndAverages(
      nonDayTradeOperationsQuantity,
      nonDayTradeOperationsAveragePrice,
      nonDayTradeOperationsAverageCost,
    )

  val operationsTradeResult: TradeResult =
    TradeResult.fromTotals(
      operationsTradeResultQuantity,
      operationsTradeResultTotalGrossValue,
      operationsTradeResultTotalCost,
    )

  val resultingPosition: AmountWithCost =
    AmountWithCost.fromSignedQuantityAndAverages(
      resultingPositionQuantity,
      resultingPositionAveragePrice,
      resultingPositionAverageCost,
    )

  def swingTradeResult: TradeResult =
    eventTradeResult + operationsTradeResult

  def withPreviousPosition(previousPosition: AmountWithCost): AssetChange =
    this.copy(
      previousPositionQuantity = previousPosition.quantity,
      previousPositionAveragePrice = previousPosition.averagePrice,
      previousPositionAverageCost = previousPosition.averageCost
    )

  def withEventTradeResult(eventTradeResult: TradeResult): AssetChange =
    this.copy(
      eventTradeResultQuantity = eventTradeResult.quantity,
      eventTradeResultTotalGrossValue = eventTradeResult.totalGrossValue,
      eventTradeResultTotalCost = eventTradeResult.totalCost,
    )

  def withPostEventPosition(postEventPosition: AmountWithCost): AssetChange =
    this.copy(
      postEventPositionQuantity = postEventPosition.quantity,
      postEventPositionAveragePrice = postEventPosition.averagePrice,
      postEventPositionAverageCost = postEventPosition.averageCost,
    )

  def withPurchaseAmount(purchaseAmount: PurchaseAmountWithCost): AssetChange =
    this.copy(
      purchaseQuantity = purchaseAmount.quantity,
      purchaseAveragePrice = purchaseAmount.averagePrice,
      purchaseAverageCost = purchaseAmount.averageCost,
    )

  def withSaleAmount(saleAmount: SaleAmountWithCost): AssetChange =
    this.copy(
      saleQuantity = saleAmount.quantity,
      saleAveragePrice = saleAmount.averagePrice,
      saleAverageCost = saleAmount.averageCost,
    )

  def withDayTradeResult(dayTradeResult: TradeResult): AssetChange =
    this.copy(
      dayTradeResultQuantity = dayTradeResult.quantity,
      dayTradeResultTotalGrossValue = dayTradeResult.totalGrossValue,
      dayTradeResultTotalCost = dayTradeResult.totalCost,
    )

  def withNonDayTradeAmount(nonDayTradeAmount: AmountWithCost): AssetChange =
    this.copy(
      nonDayTradeOperationsQuantity = nonDayTradeAmount.quantity,
      nonDayTradeOperationsAveragePrice = nonDayTradeAmount.averagePrice,
      nonDayTradeOperationsAverageCost = nonDayTradeAmount.averageCost,
    )

  def withOperationsTradeResult(operationsTradeResult: TradeResult): AssetChange =
    this.copy(
      operationsTradeResultQuantity = operationsTradeResult.quantity,
      operationsTradeResultTotalGrossValue = operationsTradeResult.totalGrossValue,
      operationsTradeResultTotalCost = operationsTradeResult.totalCost,
    )

  def withResultingPosition(resultingPosition: AmountWithCost): AssetChange =
    this.copy(
      resultingPositionQuantity = resultingPosition.signedQuantity,
      resultingPositionAveragePrice = resultingPosition.averagePrice,
      resultingPositionAverageCost = resultingPosition.averageCost,
    )
}

object AssetChange extends LocalDateSupport {
  def withZeroes(stockbrokerAsset: StockbrokerAsset, date: LocalDate, byEvent: Boolean): AssetChange =
    withZeroes(stockbrokerAsset.asset, stockbrokerAsset.stockbroker, date, byEvent)

  def withZeroes(asset: String, stockbroker: String, date: LocalDate, byEvent: Boolean): AssetChange =
    AssetChange(
      asset, stockbroker, date, byEvent,
      previousPositionQuantity = 0, previousPositionAveragePrice = 0.0, previousPositionAverageCost = 0.0,
      eventTradeResultQuantity = 0, eventTradeResultTotalGrossValue = 0.0, eventTradeResultTotalCost = 0.0,
      postEventPositionQuantity = 0, postEventPositionAveragePrice = 0.0, postEventPositionAverageCost = 0.0,
      purchaseQuantity = 0, purchaseAveragePrice = 0.0, purchaseAverageCost = 0.0,
      saleQuantity = 0, saleAveragePrice = 0.0, saleAverageCost = 0.0,
      dayTradeResultQuantity = 0, dayTradeResultTotalGrossValue = 0.0, dayTradeResultTotalCost = 0.0,
      nonDayTradeOperationsQuantity = 0, nonDayTradeOperationsAveragePrice = 0.0, nonDayTradeOperationsAverageCost = 0.0,
      operationsTradeResultQuantity = 0, operationsTradeResultTotalGrossValue = 0.0, operationsTradeResultTotalCost = 0.0,
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
