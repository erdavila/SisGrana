package sisgrana
package investments.commands.funds

import java.time.LocalDate

sealed trait PreviousRecordSet {
  def date: LocalDate
  def records: Map[String, PreviousRecord]
  def missingData: Boolean
  def totalFinalBalance: Option[Double]

  def accumulatedDays: Int
  def accumulatedTotalYieldRate: Option[Double]
  def accumulatedTotalYieldResult: Option[Double]
  def accumulatedTotalBalanceChange: Option[Double]
}

case class InitialRecordSet(
  date: LocalDate,
  records: Map[String, InitialRecord],
  totalFinalBalance: Option[Double],
) extends PreviousRecordSet {
  override def missingData: Boolean = false
  override def accumulatedDays: Int = 0
  def accumulatedTotalYieldRate: Option[Double] = None
  def accumulatedTotalYieldResult: Option[Double] = None
  def accumulatedTotalBalanceChange: Option[Double] = None
}

case class RecordSet(
  date: LocalDate,
  days: Int,
  records: Map[String, Record],
  missingData: Boolean,

  // Totals
  totalYieldRate: Option[Double],
  totalYieldResult: Option[Double],
  totalInitialBalance: Option[Double],
  totalBalanceChange: Option[Double],
  totalFinalBalance: Option[Double],

  // Accumulations
  accumulatedDays: Int,
  accumulatedTotalYieldRate: Option[Double],
  accumulatedTotalYieldResult: Option[Double],
  accumulatedTotalBalanceChange: Option[Double],
) extends PreviousRecordSet
