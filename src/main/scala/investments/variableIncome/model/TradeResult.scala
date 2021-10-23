package sisgrana
package investments.variableIncome.model

case class TradeResult(
  quantity: Int,
  totalPurchaseValue: Double,
  totalPurchaseCost: Double,
  totalSaleValue: Double,
  totalSaleCost: Double,
) {
  require(quantity >= 0)
  if (quantity == 0) {
    require(totalPurchaseValue == 0.0)
    require(totalPurchaseCost == 0.0)
    require(totalSaleValue == 0.0)
    require(totalSaleCost == 0.0)
  } else {
    require(totalPurchaseValue >= 0.0)
    require(totalPurchaseCost >= 0.0)
    require(totalSaleValue >= 0.0)
    require(totalSaleCost >= 0.0)
  }

  lazy val totalGrossValue: Double = totalSaleValue - totalPurchaseValue
  lazy val totalCost: Double = totalPurchaseCost + totalSaleCost
  lazy val totalNetValue: Double = totalGrossValue - totalCost

  def purchaseAmount: PurchaseAmount =
    PurchaseAmount.fromTotals(quantity, totalPurchaseValue, totalPurchaseCost)

  def saleAmount: SaleAmount =
    SaleAmount.fromTotals(quantity, totalSaleValue, totalSaleCost)

  def +(other: TradeResult): TradeResult = {
    TradeResult(
      this.quantity + other.quantity,
      this.totalPurchaseValue + other.totalPurchaseValue,
      this.totalPurchaseCost + other.totalPurchaseCost,
      this.totalSaleValue + other.totalSaleValue,
      this.totalSaleCost + other.totalSaleCost,
    )
  }
}

object TradeResult {
  val Zero: TradeResult = TradeResult(0, 0.0, 0.0, 0.0, 0.0)

  def from(purchase: PurchaseAmount, sale: SaleAmount): (TradeResult, Amount) = {
    val tradeQuantity = math.min(purchase.quantity, sale.quantity)
    val tradeResult = if (tradeQuantity == 0) {
      TradeResult.Zero
    } else {
      TradeResult.fromAverages(
        tradeQuantity,
        purchase.averagePrice,
        purchase.averageCost,
        sale.averagePrice,
        sale.averageCost,
      )
    }

    val largestAmount = if (purchase.quantity >= sale.quantity) purchase else sale
    val remainingAmount = largestAmount.withQuantity(largestAmount.quantity - tradeQuantity)

    (tradeResult, remainingAmount)
  }

  private def apply(
    quantity: Int,
    totalPurchaseValue: Double,
    totalPurchaseCost: Double,
    totalSaleValue: Double,
    totalSaleCost: Double,
  ): TradeResult =
    new TradeResult(quantity, totalPurchaseValue, totalPurchaseCost, totalSaleValue, totalSaleCost)

  def fromAverages(
    quantity: Int,
    averagePurchasePrice: Double,
    averagePurchaseCost: Double,
    averageSalePrice: Double,
    averageSaleCost: Double,
  ): TradeResult =
    TradeResult.fromTotals(
      quantity,
      quantity * averagePurchasePrice,
      quantity * averagePurchaseCost,
      quantity * averageSalePrice,
      quantity * averageSaleCost,
    )

  def fromTotals(
    quantity: Int,
    totalPurchaseValue: Double,
    totalPurchaseCost: Double,
    totalSaleValue: Double,
    totalSaleCost: Double,
  ): TradeResult =
    TradeResult(quantity, totalPurchaseValue, totalPurchaseCost, totalSaleValue, totalSaleCost)
}
