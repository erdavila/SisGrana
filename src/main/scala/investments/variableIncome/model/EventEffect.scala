package sisgrana
package investments.variableIncome.model

sealed trait EventEffect

object EventEffect {
  case class SetPosition(position: Amount) extends EventEffect
  case class AddToPosition(increaseAmount: PurchaseAmount, decreaseAmount: SaleAmount) extends EventEffect

  object SetPosition {
    val Type = "SetPosition"
  }

  object AddToPosition {
    val Type = "AddToPosition"
  }
}
