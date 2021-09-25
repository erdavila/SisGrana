package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{Amount, EventEffect, PurchaseAmount, SaleAmount}

sealed trait EventOutcome {
  def toEffect: EventEffect
}

object EventOutcome {
  case class SetPosition(amount: Amount, convertedToAsset: String, convertedToQuantity: Double) extends EventOutcome {
    override def toEffect: EventEffect = EventEffect.SetPosition(amount, convertedToAsset, convertedToQuantity)
  }

  case class AddToPosition(increaseAmount: PurchaseAmount, decreaseAmount: SaleAmount) extends EventOutcome {
    override def toEffect: EventEffect = EventEffect.AddToPosition(increaseAmount, decreaseAmount)
  }

  object AddToPosition {
    def apply(amount: Amount): AddToPosition =
      amount match {
        case p@PurchaseAmount(_, _, _) => AddToPosition(p, SaleAmount.Zero)
        case s@SaleAmount(_, _, _) => AddToPosition(PurchaseAmount.Zero, s)
      }
  }
}
