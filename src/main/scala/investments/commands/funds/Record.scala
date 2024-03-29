package sisgrana
package investments.commands.funds

import investments.Rate

case class Record(
  position: Option[Presence[Record.Position]],
  accumulated: Record.Accumulated,
)

object Record {
  case class Position(
    sharePrice: Double,
    yieldRate: Option[Rate],
    yieldResult: Option[Double],
    initialBalance: Option[Double],
    shareAmountChange: Option[BigDecimal],
    balanceChange: Option[Double],
    shareAmount: Option[BigDecimal],
    finalBalance: Option[Double],
    note: Option[String],
  ) extends Record.Position.Previous

  object Position {
    case class Initial(
      sharePrice: Double,
      shareAmount: Some[BigDecimal],
      finalBalance: Some[Double],
      note: Option[String],
    ) extends Record.Position.Previous

    sealed trait Previous {
      def sharePrice: Double
      def shareAmount: Option[BigDecimal]
      def finalBalance: Option[Double]
    }
  }

  case class Accumulated(
    yieldRate: Option[Rate],
    yieldResult: Option[Double],
    shareAmountChange: Option[BigDecimal],
    balanceChange: Option[Double],
    missingData: Boolean,
  )
}
