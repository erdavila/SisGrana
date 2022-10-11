package sisgrana
package investments.commands.funds

sealed trait PreviousRecord {
  def missingData: Boolean
  def sharePrice: Option[Double]
  def shareAmount: Option[BigDecimal]
  def finalBalance: Option[Double]
}

case class InitialRecord(
  sharePrice: Some[Double],
  shareAmount: Some[BigDecimal],
  finalBalance: Some[Double],
  note: Option[String],
) extends PreviousRecord {
  override def missingData: Boolean = false
}

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
  ) extends PreviousRecord

  case class Accumulated(
    days: Int,
    yieldRate: Option[Double],
    yieldResult: Option[Double],
    shareAmountChange: Option[BigDecimal],
    balanceChange: Option[Double],
    missingData: Boolean,
  )
}
