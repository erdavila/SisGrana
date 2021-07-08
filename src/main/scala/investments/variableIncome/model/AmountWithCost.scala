package sisgrana
package investments.variableIncome.model

sealed trait AmountWithCost {
  require(quantity >= 0)
  require(totalValue >= 0.0)
  require(totalCost >= 0.0)

  def quantity: Int
  def totalValue: Double
  def totalCost: Double

  lazy val averagePrice: Double = totalValue / quantity
  lazy val averageCost: Double = totalCost / quantity
  lazy val averagePriceWithCost: Double = totalValueWithCost / quantity
  def totalValueWithCost: Double

  def signedQuantity: Int = sign * quantity
  def signedTotalValue: Double = sign * totalValue
  def signedTotalValueWithCost: Double = sign * totalValueWithCost
  protected def sign: Int

  def withQuantity(quantity: Int): AmountWithCost
  protected final def withQuantity[A <: AmountWithCost](quantity: Int, zero: => A, create: (Int, Double, Double) => A): A = {
    if (this.quantity == 0) require(quantity == 0)
    if (quantity == 0) {
      zero
    } else {
      create(quantity, quantity * averagePrice, quantity * averageCost)
    }
  }

  def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): AmountWithCost
}

case class PurchaseAmountWithCost(quantity: Int, totalValue: Double, totalCost: Double) extends AmountWithCost {
  override lazy val totalValueWithCost: Double = totalValue + totalCost

  override protected def sign: Int = +1

  override def withQuantity(quantity: Int): PurchaseAmountWithCost =
    withQuantity(quantity, PurchaseAmountWithCost.Zero, PurchaseAmountWithCost(_, _, _))

  override def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost =
    PurchaseAmountWithCost(quantity, totalValue, totalCost)

  def +(other: PurchaseAmountWithCost): PurchaseAmountWithCost =
    PurchaseAmountWithCost(
      quantity = this.quantity + other.quantity,
      totalValue = this.totalValue + other.totalValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object PurchaseAmountWithCost {
  val Zero: PurchaseAmountWithCost = PurchaseAmountWithCost(0, 0.0, 0.0)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): PurchaseAmountWithCost =
    fromTotals(
      quantity,
      quantity * averagePrice,
      quantity * averageCost,
    )

  def fromTotals(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost =
    PurchaseAmountWithCost(quantity, totalValue, totalCost)
}

case class SaleAmountWithCost(quantity: Int, totalValue: Double, totalCost: Double) extends AmountWithCost {
  override lazy val totalValueWithCost: Double = totalValue - totalCost

  override protected def sign: Int = -1

  override def withQuantity(quantity: Int): SaleAmountWithCost =
    withQuantity(quantity, SaleAmountWithCost.Zero, SaleAmountWithCost(_, _, _))

  override def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost =
    SaleAmountWithCost(quantity, totalValueWithCost, totalCost)

  def +(other: SaleAmountWithCost): SaleAmountWithCost =
    SaleAmountWithCost(
      quantity = this.quantity + other.quantity,
      totalValue = this.totalValue + other.totalValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object SaleAmountWithCost {
  val Zero: SaleAmountWithCost = SaleAmountWithCost(0, 0.0, 0.0)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): SaleAmountWithCost =
    fromTotals(
      quantity,
      quantity * averagePrice,
      quantity * averageCost,
    )

  def fromTotals(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost =
    SaleAmountWithCost(quantity, totalValue, totalCost)
}
