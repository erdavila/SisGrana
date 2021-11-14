package sisgrana
package investments.fileTypes.brokerageNotes

import java.time.LocalDate

sealed trait Operation

object Operation {
  case object Purchase extends Operation
  case object Sale extends Operation
}

case class Cost(name: String, value: Double) {
  require(value >= 0.0)
}

case class Negotiation(operation: Operation, asset: String, quantity: Int, price: Double, optionAsset: Option[String]) {
  require(quantity > 0)
  require(price >= 0.0)

  lazy val totalValue: Double = quantity * price
}

case class BrokerageNote(
  stockbroker: String,
  date: LocalDate,
  negotiations: List[Negotiation],
  costs: List[Cost],
  totalValue: Double,
) {
  lazy val totalCosts: Double = costs.map(_.value).sum

  def subjectAssets: Set[String] =
    negotiations.flatMap(n => n.asset +: n.optionAsset.toSeq).toSet
}
