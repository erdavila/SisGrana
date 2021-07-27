package sisgrana
package investments.variableIncome.importAssets

import investments.utils.BrNumber
import java.io.File

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

  def parseLineValues(lineValues: Seq[String]): Event = {
    SSV.matchValues(lineValues) {
      case Seq("convert", fromAsset, fromQty, "->", toQty, toAsset) =>
        Conversion(fromAsset, BrNumber.parse(fromQty), toAsset, BrNumber.parse(toQty))
      case Seq("bonus", fromAsset, fromQty, "->", toQty, toAsset, toPrice) =>
        Bonus(fromAsset, BrNumber.parse(fromQty), toAsset, BrNumber.parse(toQty), BrNumber.parse(toPrice))
    }
  }

  def fromFile(file: File): Seq[Event] =
    SSV.readFile(file).map(Event.parseLineValues)
}
