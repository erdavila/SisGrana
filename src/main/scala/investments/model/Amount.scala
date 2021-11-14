package sisgrana
package investments.model

sealed trait Amount {
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

  protected def sign: Int

  final lazy val averagePriceWithCost: Double = averagePrice + sign * averageCost

  final lazy val grossValue: Double = averagePrice * quantity
  final lazy val totalCost: Double = averageCost * quantity
  final lazy val netValue: Double = averagePriceWithCost * quantity

  final def signedQuantity: Int = sign * quantity
  final def signedAveragePrice: Double = sign * averagePrice
  final def signedAveragePriceWithCost: Double = sign * averagePriceWithCost

  final def signedGrossValue: Double = sign * grossValue
  final def signedNetValue: Double = sign * netValue

  def withQuantity(quantity: Int): Amount

  protected final def withQuantity[A <: Amount](quantity: Int, zero: => A, fromAverages: (Int, Double, Double) => A): A = {
    if (this.quantity == 0) require(quantity == 0)
    if (quantity == 0) {
      zero
    } else {
      fromAverages(quantity, averagePrice, averageCost)
    }
  }
}

object Amount {
  val Zero: Amount = PurchaseAmount.Zero

  def fromSignedQuantityAndAverages(signedQuantity: Int, averagePrice: Double, averageCost: Double): Amount =
    if (signedQuantity >= 0) {
      PurchaseAmount.fromAverages(signedQuantity, averagePrice, averageCost)
    } else {
      SaleAmount.fromAverages(-signedQuantity, averagePrice, averageCost)
    }

  def fromSignedQuantityAndTotals(signedQuantity: Int, grossValue: Double, totalCost: Double): Amount =
    if (signedQuantity == 0) {
      require(grossValue == 0.0)
      require(totalCost == 0.0)
      PurchaseAmount.Zero
    } else {
      fromSignedQuantityAndAverages(
        signedQuantity,
        grossValue / signedQuantity,
        totalCost / math.abs(signedQuantity),
      )
    }

  def combine(amount1: Amount, amount2: Amount): (Amount, TradeResult) =
    (amount1, amount2) match {
      case (p1@PurchaseAmount(_, _, _), p2@PurchaseAmount(_, _, _)) => (p1 + p2, TradeResult.Zero)
      case (s1@SaleAmount(_, _, _), s2@SaleAmount(_, _, _)) => (s1 + s2, TradeResult.Zero)
      case (p@PurchaseAmount(_, _, _), s@SaleAmount(_, _, _)) => TradeResult.from(p, s).swap
      case (s@SaleAmount(_, _, _), p@PurchaseAmount(_, _, _)) => TradeResult.from(p, s).swap
    }
}

case class PurchaseAmount(quantity: Int, averagePrice: Double, averageCost: Double) extends Amount {
  override protected def sign: Int = +1

  override def withQuantity(quantity: Int): PurchaseAmount =
    withQuantity(quantity, PurchaseAmount.Zero, PurchaseAmount.fromAverages)

  def +(other: PurchaseAmount): PurchaseAmount =
    PurchaseAmount.fromTotals(
      quantity = this.quantity + other.quantity,
      grossValue = this.grossValue + other.grossValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object PurchaseAmount {
  val Zero: PurchaseAmount = PurchaseAmount(0, 0.0, 0.0)

  private def apply(quantity: Int, averagePrice: Double, averageCost: Double): PurchaseAmount =
    new PurchaseAmount(quantity, averagePrice, averageCost)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): PurchaseAmount =
    PurchaseAmount(quantity, averagePrice, averageCost)

  def fromTotals(quantity: Int, grossValue: Double, totalCost: Double): PurchaseAmount =
    if (quantity == 0) {
      require(grossValue == 0.0)
      require(totalCost == 0.0)
      Zero
    } else {
      fromAverages(
        quantity,
        grossValue / quantity,
        totalCost / quantity,
      )
    }
}

case class SaleAmount(quantity: Int, averagePrice: Double, averageCost: Double) extends Amount {
  override protected def sign: Int = -1

  override def withQuantity(quantity: Int): SaleAmount =
    withQuantity(quantity, SaleAmount.Zero, SaleAmount.fromAverages)

  def +(other: SaleAmount): SaleAmount =
    SaleAmount.fromTotals(
      quantity = this.quantity + other.quantity,
      grossValue = this.grossValue + other.grossValue,
      totalCost = this.totalCost + other.totalCost,
    )
}

object SaleAmount {
  val Zero: SaleAmount = SaleAmount(0, 0.0, 0.0)

  private def apply(quantity: Int, averagePrice: Double, averageCost: Double): SaleAmount =
    new SaleAmount(quantity, averagePrice, averageCost)

  def fromAverages(quantity: Int, averagePrice: Double, averageCost: Double): SaleAmount =
    SaleAmount(quantity, averagePrice, averageCost)

  def fromTotals(quantity: Int, grossValue: Double, totalCost: Double): SaleAmount =
    if (quantity == 0) {
      require(grossValue == 0.0)
      require(totalCost == 0.0)
      Zero
    } else {
      fromAverages(
        quantity,
        grossValue / quantity,
        totalCost / quantity,
      )
    }
}
