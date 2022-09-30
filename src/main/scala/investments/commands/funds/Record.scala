package sisgrana
package investments.commands.funds

sealed trait PreviousRecord {
  def sharePrice: Option[Double]
  def shareAmount: Option[BigDecimal]
  def finalBalance: Option[Double]

  def accumulatedDays: Int
  def accumulatedYieldRate: Option[Double]
  def accumulatedYieldResult: Option[Double]
  def accumulatedShareAmountChange: Option[BigDecimal]
  def accumulatedBalanceChange: Option[Double]
}

case class InitialRecord(
  sharePrice: Some[Double],
  shareAmount: Some[BigDecimal],
  finalBalance: Some[Double],
  note: Option[String],
) extends PreviousRecord {
  override def accumulatedDays: Int = 0
  override def accumulatedYieldRate: Option[Double] = None
  override def accumulatedYieldResult: Option[Double] = None
  override def accumulatedShareAmountChange: Option[BigDecimal] = None
  override def accumulatedBalanceChange: Option[Double] = None
}

case class Record(
  sharePrice: Option[Double],
  yieldRate: Option[Double],
  yieldResult: Option[Double],
  initialBalance: Option[Double],
  shareAmountChange: Option[BigDecimal],
  balanceChange: Option[Double],
  shareAmount: Option[BigDecimal],
  finalBalance: Option[Double],
  note: Option[String],

  // Accumulated
  accumulatedDays: Int,
  accumulatedYieldRate: Option[Double],
  accumulatedYieldResult: Option[Double],
  accumulatedShareAmountChange: Option[BigDecimal],
  accumulatedBalanceChange: Option[Double],
) extends PreviousRecord
