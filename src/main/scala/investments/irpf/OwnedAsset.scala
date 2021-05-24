package sisgrana
package investments.irpf

import com.softwaremill.quicklens._
import java.io.File

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
    this.modify(_.quantity).using(_ - quantity)
  }
}

object Amount {
  val Zero: Amount = Amount(0, 0.0)
}

case class OwnedAsset(stockbrokerAsset: StockbrokerAsset, amount: Amount) {
  def add(amount: Amount): OwnedAsset =
    this.modify(_.amount).using(_.add(amount))

  def remove(quantity: Int): OwnedAsset = {
    require(quantity > 0)
    this.modify(_.amount).using(_.remove(quantity))
  }
}

object OwnedAsset {
  def fromLineValues(values: Seq[String]): OwnedAsset = {
    SSV.matchValues(values) { case Seq(asset, quantityString, averagePriceString, stockbroker) =>
      val stockbrokerAsset = StockbrokerAsset(stockbroker, asset)
      val quantity = quantityString.toInt
      val averagePrice = BrNumber.parse(averagePriceString)
      OwnedAsset(stockbrokerAsset, Amount(quantity, averagePrice))
    }
  }

  def toLineValues(ownedAsset: OwnedAsset): Seq[String] =
    Vector(
      ownedAsset.stockbrokerAsset.asset,
      ownedAsset.amount.quantity,
      BrNumber.format(ownedAsset.amount.averagePrice),
      ownedAsset.stockbrokerAsset.stockbroker,
    ).map(_.toString)
}

object OwnedAssets {
  type Type = Map[StockbrokerAsset, OwnedAsset]

  def fromFile(file: File): OwnedAssets = {
    val entries = SSV.readFile(file).map { lineValues =>
      val ownedAsset = OwnedAsset.fromLineValues(lineValues)
      ownedAsset.stockbrokerAsset -> ownedAsset
    }
    entries.toMap
  }

  implicit class Ops(private val ownedAssets: OwnedAssets) extends AnyVal {
    def addTo(stockbrokerAsset: StockbrokerAsset)(amount: Amount): OwnedAssets =
      ownedAssets.updatedWith(stockbrokerAsset) { ownedAssetOpt =>
        val ownedAsset = ownedAssetOpt.getOrElse(OwnedAsset(stockbrokerAsset, Amount.Zero))
        Some(ownedAsset.add(amount))
      }

    def removeFrom(stockbrokerAsset: StockbrokerAsset)(quantity: Int): OwnedAssets = {
      assert(quantity >= 0)
      ownedAssets.updatedWith(stockbrokerAsset) { ownedAssetOpt =>
        val ownedAsset = ownedAssetOpt.getOrElse(OwnedAsset(stockbrokerAsset, Amount.Zero))
        val newOwnedAsset = ownedAsset.remove(quantity)
        Option.when(newOwnedAsset.amount.quantity > 0) { newOwnedAsset }
      }
    }

    def writeFile(file: File): Unit =
      SSV.writeFile(file)(
        ownedAssets.values.toArray
          .sortBy(oa => (oa.stockbrokerAsset.asset, oa.stockbrokerAsset.stockbroker))
          .map(OwnedAsset.toLineValues)
      )
  }
}
