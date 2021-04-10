package sisgrana
package investments.irpf

import java.time.YearMonth

case class OperationsAmounts(purchaseAmount: Amount, saleAmount: Amount)

case class ProcessedBrokerageNote(original: BrokerageNote, operationsAmountsPerAsset: Map[String, OperationsAmounts])

sealed abstract class TradeBase(quantity: Int, purchaseAveragePrice: Double, saleAveragePrice: Double) {
  require(quantity >= 0)
  require(purchaseAveragePrice >= 0.0)
  require(saleAveragePrice >= 0.0)

  lazy val purchaseValue: Double = quantity * purchaseAveragePrice
  lazy val saleValue: Double = quantity * saleAveragePrice
  lazy val result: Double = saleValue - purchaseValue
}

case class Trade(quantity: Int, purchaseAveragePrice: Double, saleAveragePrice: Double)
  extends TradeBase(quantity, purchaseAveragePrice, saleAveragePrice)
{

  def add(trade: Trade): Trade = {
    val newQuantity = this.quantity + trade.quantity
    if (newQuantity == 0) {
      Trade.Zero
    } else {
      val newPurchaseAveragePrice = (this.purchaseValue + trade.purchaseValue) / newQuantity
      val newSaleAveragePrice = (this.saleValue + trade.saleValue) / newQuantity
      Trade(newQuantity, newPurchaseAveragePrice, newSaleAveragePrice)
    }
  }
}

object Trade {
  val Zero: Trade = Trade(0, 0.0, 0.0)
}

case class TradeWithTotalSales(
  quantity: Int,
  purchaseAveragePrice: Double,
  saleAveragePrice: Double,
  totalSalesWithoutCosts: Double,
) extends TradeBase(quantity, purchaseAveragePrice, saleAveragePrice) {
  def add(tradeWithTotalSales: TradeWithTotalSales): TradeWithTotalSales = {
    val newQuantity = this.quantity + tradeWithTotalSales.quantity
    if (newQuantity == 0) {
      TradeWithTotalSales.Zero
    } else {
      TradeWithTotalSales(
        newQuantity,
        purchaseAveragePrice = (this.purchaseValue + tradeWithTotalSales.purchaseValue) / newQuantity,
        saleAveragePrice = (this.saleValue + tradeWithTotalSales.saleValue) / newQuantity,
        totalSalesWithoutCosts = this.totalSalesWithoutCosts + tradeWithTotalSales.totalSalesWithoutCosts,
      )
    }
  }
}

object TradeWithTotalSales {
  def fromTrade(trade: Trade, saleAveragePriceWithoutCosts: Double): TradeWithTotalSales =
    TradeWithTotalSales(
      quantity = trade.quantity,
      purchaseAveragePrice = trade.purchaseAveragePrice,
      saleAveragePrice = trade.saleAveragePrice,
      totalSalesWithoutCosts = trade.quantity * saleAveragePriceWithoutCosts,
    )

  val Zero: TradeWithTotalSales = TradeWithTotalSales(0, 0.0, 0.0, 0.0)
}

case class SwingTrade(exemptable: TradeWithTotalSales, nonExemptable: Trade) {
  lazy val totalQuantity: Int = exemptable.quantity + nonExemptable.quantity
  lazy val totalResult: Double = exemptable.result + nonExemptable.result
}

object SwingTrade {
  val Zero: SwingTrade = SwingTrade(TradeWithTotalSales.Zero, Trade.Zero)
}

case class YearMonthOutcome(
  yearMonth: YearMonth,
  swingTrade: SwingTrade,
  dayTrade: Trade,
  fiisTrade: Trade,
  totalIrrf: Double,
  ownedAssets: OwnedAssets,
)
