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

  purchaseQuantity: Int,
  purchaseTotalValue: Double,
  purchaseTotalCost: Double,

  saleQuantity: Int,
  saleTotalValue: Double,
  saleTotalCost: Double,

  dayTradeResultQuantity: Int,
  dayTradeResultTotalGrossValue: Double,
  dayTradeResultTotalCost: Double,

  swingTradeResultQuantity: Int,
  swingTradeResultTotalGrossValue: Double,
  swingTradeResultTotalCost: Double,

  positionQuantity: Int,
  positionTotalValue: Double,
  positionTotalCost: Double,
) {
  require(purchaseQuantity >= 0)
  require(purchaseTotalValue >= 0.0)
  require(purchaseTotalCost >= 0.0)
  require(saleQuantity >= 0)
  require(saleTotalValue >= 0.0)
  require(saleTotalCost >= 0.0)
  require(dayTradeResultQuantity >= 0)
  require(dayTradeResultTotalCost >= 0.0)
  require(swingTradeResultQuantity >= 0)
  require(swingTradeResultTotalCost >= 0.0)
  require(math.signum(positionQuantity) == math.signum(positionTotalValue))
  require(positionTotalCost >= 0.0)

  lazy val stockbrokerAsset: StockbrokerAsset =
    StockbrokerAsset(stockbroker, asset)

  lazy val purchaseAmount: PurchaseAmountWithCost =
    PurchaseAmountWithCost(purchaseQuantity, purchaseTotalValue, purchaseTotalCost)

  lazy val saleAmount: SaleAmountWithCost =
    SaleAmountWithCost(saleQuantity, saleTotalValue, saleTotalCost)

  lazy val dayTradeResult: TradeResult =
    TradeResult.fromTotals(dayTradeResultQuantity, dayTradeResultTotalGrossValue, dayTradeResultTotalCost)

  lazy val swingTradeResult: TradeResult =
    TradeResult.fromTotals(swingTradeResultQuantity, swingTradeResultTotalGrossValue, swingTradeResultTotalCost)

  lazy val position: AmountWithCost =
    if (positionQuantity >= 0) {
      PurchaseAmountWithCost(positionQuantity, positionTotalValue, positionTotalCost)
    } else {
      SaleAmountWithCost(-positionQuantity, -positionTotalValue, positionTotalCost)
    }

  def withPurchaseAmount(purchase: PurchaseAmountWithCost): AssetChange =
    this.copy(
      purchaseQuantity = purchase.quantity,
      purchaseTotalValue = purchase.totalValue,
      purchaseTotalCost = purchase.totalCost,
    )

  def withSaleAmount(sale: SaleAmountWithCost): AssetChange =
    this.copy(
      saleQuantity = sale.quantity,
      saleTotalValue = sale.totalValue,
      saleTotalCost = sale.totalCost,
    )

  def withDayTradeResult(dayTradeResult: TradeResult): AssetChange =
    this.copy(
      dayTradeResultQuantity = dayTradeResult.quantity,
      dayTradeResultTotalGrossValue = dayTradeResult.totalGrossValue,
      dayTradeResultTotalCost = dayTradeResult.totalCost,
    )

  def withSwingTradeResult(swingTradeResult: TradeResult): AssetChange =
    this.copy(
      swingTradeResultQuantity = swingTradeResult.quantity,
      swingTradeResultTotalGrossValue = swingTradeResult.totalGrossValue,
      swingTradeResultTotalCost = swingTradeResult.totalCost,
    )

  def withPosition(position: AmountWithCost): AssetChange = {
    position match {
      case PurchaseAmountWithCost(quantity, totalValue, totalCost) =>
        this.copy(
          positionQuantity = quantity,
          positionTotalValue = totalValue,
          positionTotalCost = totalCost
        )
      case SaleAmountWithCost(quantity, totalValue, totalCost) =>
        this.copy(
          positionQuantity = -quantity,
          positionTotalValue = -totalValue,
          positionTotalCost = totalCost
        )
    }
  }
}

object AssetChange extends LocalDateSupport {
  def withZeroes(stockbrokerAsset: StockbrokerAsset, date: LocalDate, byEvent: Boolean): AssetChange =
    withZeroes(stockbrokerAsset.asset, stockbrokerAsset.stockbroker, date, byEvent)

  def withZeroes(asset: String, stockbroker: String, date: LocalDate, byEvent: Boolean): AssetChange =
    AssetChange(
      asset, stockbroker, date, byEvent,
      purchaseQuantity = 0, purchaseTotalValue = 0.0, purchaseTotalCost = 0.0,
      saleQuantity = 0, saleTotalValue = 0.0, saleTotalCost = 0.0,
      dayTradeResultQuantity = 0, dayTradeResultTotalGrossValue = 0.0, dayTradeResultTotalCost = 0.0,
      swingTradeResultQuantity = 0, swingTradeResultTotalGrossValue = 0.0, swingTradeResultTotalCost = 0.0,
      positionQuantity = 0, positionTotalValue = 0.0, positionTotalCost = 0.0,
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
