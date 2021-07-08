package sisgrana
package investments.variableIncome.model

case class Amount(quantity: Int, totalValue: Double) {
  require(quantity >= 0)
  require(totalValue >= 0.0)

  lazy val averagePrice: Double = totalValue / quantity

  def +(other: Amount): Amount =
    Amount(this.quantity + other.quantity, this.totalValue + other.totalValue)
}

object Amount {
  val Zero: Amount = Amount(0, 0.0)

  def fromAveragePrice(quantity: Int, averagePrice: Double): Amount =
    Amount(quantity, quantity * averagePrice)
}
