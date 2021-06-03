package sisgrana
package investments.variableIncome.model

import investments.variableIncome.importAssets.Operation

trait AmountBase {
  def quantity: Int
  def totalValue: Double
  lazy val averagePrice: Double = totalValue / quantity
}

case class Amount(quantity: Int, totalValue: Double) extends AmountBase {
  require(quantity >= 0)
  require(totalValue >= 0.0)

  def +(other: Amount): Amount =
    Amount(this.quantity + other.quantity, this.totalValue + other.totalValue)

  def withTotalCost(totalCost: Double, operation: Operation): AmountWithCost =
    AmountWithCost(quantity, totalValue, totalCost, operation)
}

object Amount {
  val Zero: Amount = Amount(0, 0.0)

  def fromAveragePrice(quantity: Int, averagePrice: Double): Amount =
    Amount(quantity, quantity * averagePrice)
}

case class AmountWithCost(quantity: Int, totalValue: Double, totalCost: Double, operation: Operation = Operation.Purchase) extends AmountBase {
  require(quantity >= 0)
  require(totalValue >= 0.0)
  require(totalCost >= 0.0)

  lazy val totalValueWithCost: Double = totalValue + totalCost

  lazy val averagePriceWithCost: Double = {
    val total = operation match {
      case Operation.Purchase => totalValue + totalCost
      case Operation.Sale => totalValue - totalCost
    }
    total / quantity
  }

  lazy val averageCost: Double = totalCost / quantity

  def +(other: AmountWithCost): AmountWithCost =
    AmountWithCost(this.quantity + other.quantity, this.totalValue + other.totalValue, this.totalCost + other.totalCost, operation)

  def modifyQuantity(f: Int => Int): AmountWithCost = {
    val newQuantity = f(quantity)
    val newTotalValue = newQuantity * averagePrice
    val newTotalCost = newQuantity * averageCost
    AmountWithCost(newQuantity, newTotalValue, newTotalCost, operation)
  }
}

object AmountWithCost {
  val Zero: AmountWithCost = AmountWithCost(0, 0.0, 0.0, Operation.Purchase)

  def fromAveragePriceAndCost(quantity: Int, averagePrice: Double, averageCost: Double): AmountWithCost =
    AmountWithCost(quantity, quantity * averagePrice, quantity * averageCost, Operation.Purchase)
}
