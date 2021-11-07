package sisgrana
package investments.variableIncome.multiImport.eventsAndBrokerageNotes

import investments.variableIncome.model.{Amount, PurchaseAmount, SaleAmount, StockbrokerAsset}
import utils.oppositeSigns

case class AggregatedNegotiations(
  stockbrokerAsset: StockbrokerAsset,
  purchase: PurchaseAmount,
  sale: SaleAmount,
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
    AggregatedNegotiations(stockbrokerAsset, PurchaseAmount.Zero, SaleAmount.Zero, exercisedQuantity)

  def apply(stockbrokerAsset: StockbrokerAsset, amount: Amount): AggregatedNegotiations =
    amount match {
      case p@PurchaseAmount(_, _, _) => AggregatedNegotiations(stockbrokerAsset, p, SaleAmount.Zero, 0)
      case s@SaleAmount(_, _, _) => AggregatedNegotiations(stockbrokerAsset, PurchaseAmount.Zero, s, 0)
    }
}
