package sisgrana
package investments.variableIncome.model

sealed trait EventEffect

object EventEffect {
  case class SetPosition(position: Amount, /*TODO: remove*/convertedToAsset: String, /*TODO: remove*/convertedToQuantity: Double) extends EventEffect
  case class AddToPosition(increaseAmount: PurchaseAmount, decreaseAmount: SaleAmount) extends EventEffect

  object SetPosition {
    val Type = "SetPosition"
  }

  object AddToPosition {
    val Type = "AddToPosition"

    def apply(amount: Amount): AddToPosition =
      amount match {
        case p@PurchaseAmount(_, _, _) => AddToPosition(p, SaleAmount.Zero)
        case s@SaleAmount(_, _, _) => AddToPosition(PurchaseAmount.Zero, s)
      }
  }
}
