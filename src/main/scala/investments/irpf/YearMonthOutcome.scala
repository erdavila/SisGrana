package sisgrana
package investments.irpf

case class Trade(quantity: Int, purchaseAveragePrice: Double, saleAveragePrice: Double) {
  require(quantity >= 0)
  require(purchaseAveragePrice >= 0.0)
  require(saleAveragePrice >= 0.0)

  lazy val purchaseValue: Double = quantity * purchaseAveragePrice
  lazy val saleValue: Double = quantity * saleAveragePrice
  lazy val result: Double = saleValue - purchaseValue

  def purchaseAmount: Amount = Amount(quantity, purchaseAveragePrice)
  def saleAmount: Amount = Amount(quantity, saleAveragePrice)
}

object Trade {
  val Zero: Trade = Trade(0, 0.0, 0.0)
}

case class SwingTrade(exemptableResult: Double, exemptableTotalSalesWithoutCost: Double, nonExemptableResult: Double) {
  def totalResult: Double = exemptableResult + nonExemptableResult

  def + (other: SwingTrade): SwingTrade =
    SwingTrade(
      this.exemptableResult + other.exemptableResult,
      this.exemptableTotalSalesWithoutCost + other.exemptableTotalSalesWithoutCost,
      this.nonExemptableResult + other.nonExemptableResult,
    )
}

object SwingTrade {
  val Zero: SwingTrade = SwingTrade(0.0, 0.0, 0.0)
}

case class YearMonthOutcome(
  dayTradeResult: Double,
  swingTrade: SwingTrade,
  fiisResult: Double,
  ownedAssets: OwnedAssets,
)
