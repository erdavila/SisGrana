package sisgrana
package investments.variableIncome.model

sealed trait EventEffect

object EventEffect {
  case class SetPosition(position: AmountWithCost) extends EventEffect
  case class AddToPosition(increaseAmount: PurchaseAmountWithCost, decreaseAmount: SaleAmountWithCost) extends EventEffect

  object SetPosition {
    val Type = "SetPosition"
  }

  object AddToPosition {
    val Type = "AddToPosition"

    def apply(amount: AmountWithCost): AddToPosition =
      amount match {
        case p@PurchaseAmountWithCost(_, _, _) => AddToPosition(p, SaleAmountWithCost.Zero)
        case s@SaleAmountWithCost(_, _, _) => AddToPosition(PurchaseAmountWithCost.Zero, s)
      }
  }
}
