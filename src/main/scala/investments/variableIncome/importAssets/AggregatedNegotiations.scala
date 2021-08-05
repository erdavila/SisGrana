package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.model.{AmountWithCost, PurchaseAmountWithCost, SaleAmountWithCost, StockbrokerAsset}
import sisgrana.utils.oppositeSigns

case class AggregatedNegotiations(
  stockbrokerAsset: StockbrokerAsset,
  purchase: PurchaseAmountWithCost,
  sale: SaleAmountWithCost,
  exercisedQuantity: Int,
) {
  def +(other: AggregatedNegotiations): AggregatedNegotiations = {
    require(this.stockbrokerAsset == other.stockbrokerAsset)
    require(!oppositeSigns(this.exercisedQuantity, other.exercisedQuantity))

    AggregatedNegotiations(
      this.stockbrokerAsset,
      this.purchase + other.purchase,
      this.sale + other.sale,
      this.exercisedQuantity + other.exercisedQuantity,
    )
  }
}

object AggregatedNegotiations {
  def apply(stockbrokerAsset: StockbrokerAsset, exercisedQuantity: Int): AggregatedNegotiations =
    AggregatedNegotiations(stockbrokerAsset, PurchaseAmountWithCost.Zero, SaleAmountWithCost.Zero, exercisedQuantity)

  def apply(stockbrokerAsset: StockbrokerAsset, amount: AmountWithCost): AggregatedNegotiations =
    amount match {
      case p@PurchaseAmountWithCost(_, _, _) => AggregatedNegotiations(stockbrokerAsset, p, SaleAmountWithCost.Zero, 0)
      case s@SaleAmountWithCost(_, _, _) => AggregatedNegotiations(stockbrokerAsset, PurchaseAmountWithCost.Zero, s, 0)
    }
}
