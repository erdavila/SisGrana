package sisgrana
package investments.commands.funds

object Record {
  case class Position(
    missingData: Boolean,
    sharePrice: Option[Double],
    yieldRate: Option[Double],
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
      sharePrice: Some[Double],
      shareAmount: Some[BigDecimal],
      finalBalance: Some[Double],
      note: Option[String],
    ) extends Record.Position.Previous {
      override def missingData: Boolean = false
    }

    sealed trait Previous {
      def missingData: Boolean
      def sharePrice: Option[Double]
      def shareAmount: Option[BigDecimal]
      def finalBalance: Option[Double]
    }
  }

  case class Accumulated(
    days: Int,
    yieldRate: Option[Double],
    yieldResult: Option[Double],
    shareAmountChange: Option[BigDecimal],
    balanceChange: Option[Double],
    missingData: Boolean,
  )
}
