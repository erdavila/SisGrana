package sisgrana
package investments.irpf

import investments.irpf.TSV.Elements
import monocle.syntax.all._

case class Amount(quantity: Int, averagePrice: Double) {
  require(quantity >= 0)
  require(averagePrice >= 0.0)

  lazy val totalValue: Double = quantity * averagePrice

  def add(amount: Amount): Amount = {
    val newQuantity = this.quantity + amount.quantity
    val newAveragePrice = (this.totalValue + amount.totalValue) / newQuantity
    Amount(quantity = newQuantity, newAveragePrice)
  }

  def remove(quantity: Int): Amount = {
    require(quantity > 0)
    this.focus(_.quantity).modify(_ - quantity)
  }
}

object Amount {
  val Zero: Amount = Amount(0, 0.0)
}

case class OwnedAsset(stockbrokerAsset: StockbrokerAsset, amount: Amount) {
  def add(amount: Amount): OwnedAsset =
    this.focus(_.amount).modify(_.add(amount))

  def remove(quantity: Int): OwnedAsset = {
    require(quantity > 0)
    this.focus(_.amount).modify(_.remove(quantity))
  }
}

object OwnedAsset {
  def parseTsvElements(elements: Elements): OwnedAsset = {
    Elements.matching(elements) { case List(asset, quantityString, averagePriceString, stockbroker) =>
      val stockbrokerAsset = StockbrokerAsset(stockbroker, asset)
      val quantity = quantityString.toInt
      val averagePrice = BrNumber.parse(averagePriceString)
      OwnedAsset(stockbrokerAsset, Amount(quantity, averagePrice))
    }
  }

  def toTsvElements(ownedAsset: OwnedAsset): Elements =
    Elements(
      ownedAsset.stockbrokerAsset.asset,
      ownedAsset.amount.quantity,
      BrNumber.format(ownedAsset.amount.averagePrice),
      ownedAsset.stockbrokerAsset.stockbroker,
    )
}
