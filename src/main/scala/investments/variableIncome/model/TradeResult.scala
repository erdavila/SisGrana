package sisgrana
package investments.variableIncome.model

case class TradeResult(quantity: Int, totalGrossValue: Double, totalCost: Double) {
  require(quantity >= 0)
  if (quantity == 0) {
    require(totalGrossValue == 0.0)
    require(totalCost == 0.0)
  } else {
    require(totalCost >= 0.0)
  }

  lazy val netValue: Double = totalGrossValue - totalCost

  def +(other: TradeResult): TradeResult =
    TradeResult(
      this.quantity + other.quantity,
      this.totalGrossValue + other.totalGrossValue,
      this.totalCost + other.totalCost,
    )
}

object TradeResult {
  val Zero: TradeResult = TradeResult(0, 0.0, 0.0)

  def from(purchase: PurchaseAmountWithCost, sale: SaleAmountWithCost): (TradeResult, AmountWithCost) = {
    val tradeQuantity = math.min(purchase.quantity, sale.quantity)
    val tradeResult = if (tradeQuantity == 0) {
      TradeResult.Zero
    } else {
      TradeResult.fromAverages(
        tradeQuantity,
        averageGrossValue = sale.averagePrice - purchase.averagePrice,
        averageCost = sale.averageCost + purchase.averageCost,
      )
    }

    val largestAmount = if (purchase.quantity >= sale.quantity) purchase else sale
    val remainingAmount = largestAmount.withQuantity(largestAmount.quantity - tradeQuantity)

    (tradeResult, remainingAmount)
  }

  def fromAverages(quantity: Int, averageGrossValue: Double, averageCost: Double): TradeResult =
    TradeResult.fromTotals(
      quantity,
      quantity * averageGrossValue,
      quantity * averageCost,
    )

  def fromTotals(quantity: Int, totalGrossValue: Double, totalCost: Double): TradeResult =
    TradeResult(quantity, totalGrossValue, totalCost)
}
