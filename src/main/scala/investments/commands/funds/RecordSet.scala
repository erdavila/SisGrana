package sisgrana
package investments.commands.funds

import java.time.LocalDate

sealed trait PreviousRecordSet {
  def date: LocalDate
  def positionRecords: Map[String, Record.Position.Previous]
  def missingData: Boolean
  def totalFinalBalance: Option[Double]
}

case class InitialRecordSet(
  date: LocalDate,
  positionRecords: Map[String, Record.Position.Initial],
  totalFinalBalance: Option[Double],
) extends PreviousRecordSet {
  override def missingData: Boolean = false
}

case class RecordSet(
  date: LocalDate,
  days: Int,
  positionRecords: Map[String, Record.Position],
  missingData: Boolean,

  // Totals
  totalYieldRate: Option[Double],
  totalYieldResult: Option[Double],
  totalInitialBalance: Option[Double],
  totalBalanceChange: Option[Double],
  totalFinalBalance: Option[Double],
) extends PreviousRecordSet

object RecordSet {
  case class Accumulated(
    days: Int,
    records: Map[String, Record.Accumulated],
    totalYieldRate: Option[Double],
    totalYieldResult: Option[Double],
    totalBalanceChange: Option[Double],
    missingData: Boolean,
  )
}
