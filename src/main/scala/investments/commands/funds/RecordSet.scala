package sisgrana
package investments.commands.funds

import java.time.LocalDate

sealed trait PreviousRecordSet {
  def date: LocalDate
  def records: Map[String, PreviousRecord]
  def totalFinalBalance: Option[Double]
}

case class InitialRecordSet(
  date: LocalDate,
  records: Map[String, InitialRecord],
  totalFinalBalance: Option[Double],
) extends PreviousRecordSet

case class RecordSet(
  date: LocalDate,
  days: Int,
  records: Map[String, Record],

  // Totals
  totalYieldRate: Option[Double],
  totalYieldResult: Option[Double],
  totalInitialBalance: Option[Double],
  totalBalanceChange: Option[Double],
  totalFinalBalance: Option[Double],
) extends PreviousRecordSet
