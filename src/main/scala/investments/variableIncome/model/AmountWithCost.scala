package sisgrana
package investments.variableIncome.model

sealed trait AmountWithCost {
  require(quantity >= 0)
  if (quantity == 0) {
    require(averagePrice == 0.0)
    require(averageCost == 0.0)
  } else {
    require(averagePrice >= 0.0)
    require(averageCost >= 0.0)
  }

  def quantity: Int
  def averagePrice: Double
  def averageCost: Double

  final lazy val totalValue: Double = averagePrice * quantity
  final lazy val totalCost: Double = averageCost * quantity
  final lazy val totalValueWithCost: Double = averagePriceWithCost * quantity
  def averagePriceWithCost: Double

  final def signedQuantity: Int = sign * quantity
  final def signedTotalValue: Double = sign * totalValue
  final def signedTotalValueWithCost: Double = sign * totalValueWithCost
  final def signedAveragePrice: Double = signedTotalValue / quantity
  protected def sign: Int

  def withQuantity(quantity: Int): AmountWithCost
  protected final def withQuantity[A <: AmountWithCost](quantity: Int, zero: => A, fromAverages: (Int, Double, Double) => A): A = {
    if (this.quantity == 0) require(quantity == 0)
    if (quantity == 0) {
      zero
    } else {
      fromAverages(quantity, averagePrice, averageCost)
    }
  }

  def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): AmountWithCost
}

object AmountWithCost {
  val Zero: AmountWithCost = PurchaseAmountWithCost.Zero

  def fromSignedQuantityAndAverages(signedQuantity: Int, averagePrice: Double, averageCost: Double): AmountWithCost =
    if (signedQuantity >= 0) {
      PurchaseAmountWithCost.fromAverages(signedQuantity, averagePrice, averageCost)
    } else {
      SaleAmountWithCost.fromAverages(-signedQuantity, averagePrice, averageCost)
    }

  def fromSignedQuantityAndTotals(signedQuantity: Int, totalValue: Double, totalCost: Double): AmountWithCost =
    if (signedQuantity == 0) {
      require(totalValue == 0.0)
      require(totalCost == 0.0)
      PurchaseAmountWithCost.Zero
    } else {
      fromSignedQuantityAndAverages(
        signedQuantity,
        totalValue / signedQuantity,
        totalCost / math.abs(signedQuantity),
      )
    }

  def combine(amount1: AmountWithCost, amount2: AmountWithCost): (AmountWithCost, TradeResult) =
    (amount1, amount2) match {
      case (p1@PurchaseAmountWithCost(_, _, _), p2@PurchaseAmountWithCost(_, _, _)) => (p1 + p2, TradeResult.Zero)
      case (s1@SaleAmountWithCost(_, _, _), s2@SaleAmountWithCost(_, _, _)) => (s1 + s2, TradeResult.Zero)
      case (p@PurchaseAmountWithCost(_, _, _), s@SaleAmountWithCost(_, _, _)) => TradeResult.from(p, s).swap
      case (s@SaleAmountWithCost(_, _, _), p@PurchaseAmountWithCost(_, _, _)) => TradeResult.from(p, s).swap
    }
}

case class PurchaseAmountWithCost(quantity: Int, averagePrice: Double, averageCost: Double) extends AmountWithCost {
  override lazy val averagePriceWithCost: Double = averagePrice + averageCost

  override protected def sign: Int = +1

  override def withQuantity(quantity: Int): PurchaseAmountWithCost =
    withQuantity(quantity, PurchaseAmountWithCost.Zero, PurchaseAmountWithCost.fromAverages)

  override def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost =
    PurchaseAmountWithCost(quantity, totalValue, totalCost)

  def +(other: PurchaseAmountWithCost): PurchaseAmountWithCost =
    PurchaseAmountWithCost.fromTotals(
      quantity = this.quantity + other.quantity,
      totalValue = this.totalValue + other.totalValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object PurchaseAmountWithCost {
  val Zero: PurchaseAmountWithCost = PurchaseAmountWithCost(0, 0.0, 0.0)

  private def apply(quantity: Int, averagePrice: Double, averageCost: Double): PurchaseAmountWithCost =
    new PurchaseAmountWithCost(quantity, averagePrice, averageCost)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): PurchaseAmountWithCost =
    PurchaseAmountWithCost(quantity, averagePrice, averageCost)

  def fromTotals(quantity: Int, totalValue: Double, totalCost: Double): PurchaseAmountWithCost =
    if (quantity == 0) {
      require(totalValue == 0.0)
      require(totalCost == 0.0)
      Zero
    } else {
      fromAverages(
        quantity,
        totalValue / quantity,
        totalCost / quantity,
      )
    }
}

case class SaleAmountWithCost(quantity: Int, averagePrice: Double, averageCost: Double) extends AmountWithCost {
  override lazy val averagePriceWithCost: Double = averagePrice - averageCost

  override protected def sign: Int = -1

  override def withQuantity(quantity: Int): SaleAmountWithCost =
    withQuantity(quantity, SaleAmountWithCost.Zero, SaleAmountWithCost.fromAverages)

  override def withSameOperation(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost =
    SaleAmountWithCost(quantity, totalValueWithCost, totalCost)

  def +(other: SaleAmountWithCost): SaleAmountWithCost =
    SaleAmountWithCost.fromTotals(
      quantity = this.quantity + other.quantity,
      totalValue = this.totalValue + other.totalValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object SaleAmountWithCost {
  val Zero: SaleAmountWithCost = SaleAmountWithCost(0, 0.0, 0.0)

  private def apply(quantity: Int, averagePrice: Double, averageCost: Double): SaleAmountWithCost =
    new SaleAmountWithCost(quantity, averagePrice, averageCost)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): SaleAmountWithCost =
    SaleAmountWithCost(quantity, averagePrice, averageCost)

  def fromTotals(quantity: Int, totalValue: Double, totalCost: Double): SaleAmountWithCost =
    if (quantity == 0) {
      require(totalValue == 0.0)
      require(totalCost == 0.0)
      Zero
    } else {
      fromAverages(
        quantity,
        totalValue / quantity,
        totalCost / quantity,
      )
    }
}
