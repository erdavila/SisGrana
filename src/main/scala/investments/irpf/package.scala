package sisgrana
package investments

import java.io.File

package object irpf {
  type OwnedAssets = Map[StockbrokerAsset, OwnedAsset]

  object OwnedAssets {
    def fromFile(file: File): OwnedAssets = {
      val entries = SSV.readFile(file).map { lineValues =>
        val ownedAsset = OwnedAsset.fromLineValues(lineValues)
        ownedAsset.stockbrokerAsset -> ownedAsset
      }
      entries.toMap
    }
  }

  implicit class OwnedAssetsOps(private val ownedAssets: OwnedAssets) extends AnyVal {
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

  def use[A <: AutoCloseable, B](a: A)(f: A => B): B =
    try {
      f(a)
    } finally {
      a.close()
    }

  def quoted(any: Any): String =
    '"'.toString + any.toString + '"'.toString
}
