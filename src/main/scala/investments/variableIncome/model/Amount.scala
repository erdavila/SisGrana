package sisgrana
package investments.variableIncome.model

case class Amount(quantity: Int, averagePrice: Double) {
  require(quantity >= 0)
  if (quantity == 0) {
    require(averagePrice == 0.0)
  } else {
    require(averagePrice > 0.0)
  }

  lazy val totalValue: Double = averagePrice * quantity

  def +(other: Amount): Amount =
    Amount.fromTotalValue(this.quantity + other.quantity, this.totalValue + other.totalValue)
}

object Amount {
  val Zero: Amount = Amount(0, 0.0)

  private def apply(quantity: Int, averagePrice: Double): Amount =
    new Amount(quantity, averagePrice)

  def fromTotalValue(quantity: Int, totalValue: Double): Amount =
    if (quantity == 0) {
      require(totalValue == 0.0)
      Zero
    } else {
      Amount.fromAveragePrice(quantity, totalValue / quantity)
    }

  def fromAveragePrice(quantity: Int, averagePrice: Double): Amount =
    Amount(quantity, averagePrice)
}
