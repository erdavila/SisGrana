package sisgrana
package investments.fileTypes.events

sealed trait Event {
  def subjectAssets: Set[String]
}

object Event {
  case class Conversion(fromAsset: String, fromQuantity: Double, toAsset: String, toQuantity: Double) extends Event {
    require(fromQuantity > 0.0)
    require(toQuantity > 0.0)
    override def subjectAssets: Set[String] = Set(fromAsset, toAsset)
  }

  case class Bonus(fromAsset: String, fromQuantity: Double, toAsset: String, toQuantity: Double, toPrice: Double) extends Event {
    require(fromQuantity > 0.0)
    require(toQuantity > 0.0)
    require(toPrice > 0.0)
    override def subjectAssets: Set[String] = Set(fromAsset, toAsset)
  }

  case class Transference(asset: String, fromStockbroker: String, toStockbroker: String) extends Event {
    require(fromStockbroker != toStockbroker)
    override def subjectAssets: Set[String] = Set(asset)
  }
}
