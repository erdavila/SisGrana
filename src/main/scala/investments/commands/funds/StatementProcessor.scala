package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.{LocalDate, YearMonth}
import utils.Traversing._

object StatementProcessor {
  private val ZeroRecordSetAccumulated = RecordSet.Accumulated(
    days = 0,
    records = Map.empty,
    totalYieldRate = None,
    totalYieldResult = None,
    totalBalanceChange = None,
    missingData = false,
  )

  def process(yearMonth: YearMonth, statement: FundsStatement): (InitialRecordSet, Seq[RecordSet.Position], RecordSet.Accumulated) = {
    val initialRecords = statement.initialEntries
      .view.mapValues(initialPositionRecordFrom)
      .toMap

    val initialDate = yearMonth.atDay(1).minusDays(1)
    val initialRecordSet = InitialRecordSet(
      date = initialDate,
      positionRecords = initialRecords,
      totalFinalBalance = sumIfAny(initialRecords.values.flatMap(_.finalBalance))
    )
    implicit val daysCounter: DaysCounter = new DaysCounter(statement.noPriceDates)

    val ((_, recordSetAccumulated), remainingDaysPositionRecords) = statement.entries
      .toSeq.sortBy { case (date, _) => date }
      .foldFlatMapLeft((initialRecordSet: PreviousRecordSet, ZeroRecordSetAccumulated)) { case ((previousRecordSet, recordSetAccumulated), (date, entries)) =>
        val positionRecordSet = positionRecordSetFrom(entries, date, previousRecordSet)
        val updatedRecordSetAccumulated = recordSetAccumulatedFrom(recordSetAccumulated, positionRecordSet)
        ((positionRecordSet, updatedRecordSetAccumulated), Some(positionRecordSet))
      }

    (initialRecordSet, remainingDaysPositionRecords, recordSetAccumulated)
  }

  private def initialPositionRecordFrom(initialEntry: FundsStatement.InitialEntry): Record.Position.Initial =
    Record.Position.Initial(
      sharePrice = Some(initialEntry.sharePrice),
      shareAmount = Some(initialEntry.shareAmount),
      finalBalance = Some(initialEntry.shareAmount.toDouble * initialEntry.sharePrice),
      note = initialEntry.note,
    )

  private def positionRecordSetFrom(dayEntries: Map[String, FundsStatement.Entry], date: LocalDate, previousRecordSet: PreviousRecordSet)(implicit daysCounter: DaysCounter): RecordSet.Position = {
    val days = daysCounter.count(previousRecordSet.date, date)
    val positionRecords = positionRecordsFrom(dayEntries, previousRecordSet)
    positionRecordSetFrom(positionRecords, date, days, previousRecordSet)
  }

  private def positionRecordsFrom(dayEntries: Map[String, FundsStatement.Entry], previousRecordSet: PreviousRecordSet): Map[String, Record.Position] =
    (dayEntries.keys ++ previousRecordSet.positionRecords.keys).toSeq
      .distinct
      .map { fund =>
        val entry = dayEntries.get(fund)
        val previousRecord = previousRecordSet.positionRecords.get(fund)
        fund -> positionRecordFrom(entry, previousRecord)
      }
      .toMap

  private[funds] def positionRecordSetFrom(positionRecords: Map[String, Record.Position], date: LocalDate, days: Int, previousRecordSet: PreviousRecordSet): RecordSet.Position = {
    val totalInitialBalance = sumIfAny(positionRecords.values.flatMap(_.initialBalance))
    val totalYieldRate = (totalInitialBalance, previousRecordSet.totalFinalBalance).mapN(_ / _ - 1)
    val totalYieldResult = sumIfAny(positionRecords.values.flatMap(_.yieldResult))
    val totalBalanceChange = sumIfAny(positionRecords.values.flatMap(_.balanceChange))

    RecordSet.Position(
      date = date,
      days = days,
      positionRecords = positionRecords,
      missingData = previousRecordSet.missingData || positionRecords.values.exists(_.missingData),
      totalYieldRate = totalYieldRate,
      totalYieldResult = totalYieldResult,
      totalInitialBalance = totalInitialBalance,
      totalBalanceChange = totalBalanceChange,
      totalFinalBalance = sumIfAny(positionRecords.values.flatMap(_.finalBalance)),
    )
  }

  private[funds] def positionRecordFrom(entry: Option[FundsStatement.Entry], previousPositionRecord: Option[Record.Position.Previous]): Record.Position = {
    val previousMissingData = previousPositionRecord.map(_.missingData)
    val previousSharePrice = previousPositionRecord.flatMap(_.sharePrice)
    val previousFinalBalance = previousPositionRecord.flatMap(_.finalBalance)
    val previousShareAmount = previousPositionRecord.flatMap(_.shareAmount)

    val sharePrice = entry.map(_.sharePrice)
    val yieldRate = (sharePrice, previousSharePrice).mapN(_ / _ - 1)
    val yieldResult = (previousFinalBalance, yieldRate).mapN(_ * _)
    val shareAmountChange = entry.flatMap(_.shareAmountChange)
    val balanceChange = (shareAmountChange, sharePrice).mapN(_.toDouble * _)
    val shareAmount = Some((previousShareAmount ++ shareAmountChange).sum).filter(_ != 0)

    Record.Position(
      missingData = previousMissingData.getOrElse(false) || (sharePrice.isEmpty && shareAmount.nonEmpty),
      sharePrice = sharePrice,
      yieldRate = yieldRate,
      yieldResult = yieldResult,
      initialBalance = (previousShareAmount, sharePrice).mapN(_.toDouble * _),
      shareAmountChange = shareAmountChange,
      balanceChange = balanceChange,
      shareAmount = shareAmount,
      finalBalance = (shareAmount, sharePrice).mapN(_.toDouble * _),
      note = entry.flatMap(_.note),
    )
  }

  private def recordSetAccumulatedFrom(previousRecordSetAccumulated: RecordSet.Accumulated, positionRecordSet: RecordSet.Position): RecordSet.Accumulated = {
    val currentRecordSetAccumulated = toRecordSetAccumulated(positionRecordSet)
    sumRecordSetsAccumulated(previousRecordSetAccumulated, currentRecordSetAccumulated)
  }

  private def toRecordSetAccumulated(positionRecordSet: RecordSet.Position): RecordSet.Accumulated =
    RecordSet.Accumulated(
      days = positionRecordSet.days,
      records = positionRecordSet.positionRecords
        .view
        .mapValues(positionRecord =>
          toRecordAccumulated(positionRecord, positionRecordSet.days)
        )
        .toMap,
      totalYieldRate = positionRecordSet.totalYieldRate,
      totalYieldResult = positionRecordSet.totalYieldResult,
      totalBalanceChange = positionRecordSet.totalBalanceChange,
      missingData = positionRecordSet.missingData,
    )

  private def toRecordAccumulated(positionRecord: Record.Position, days: Int): Record.Accumulated =
    Record.Accumulated(
      days = positionRecord.yieldRate.fold(0)(_ => days),
      yieldRate = positionRecord.yieldRate,
      yieldResult = positionRecord.yieldResult,
      shareAmountChange = positionRecord.shareAmountChange,
      balanceChange = positionRecord.balanceChange,
      missingData = positionRecord.missingData,
    )

  def sumRecordSetsAccumulated(recordSetsAccumulated: Iterable[RecordSet.Accumulated]): RecordSet.Accumulated =
    recordSetsAccumulated
      .reduceOption((rSetAcc1, rSetAcc2) =>
        sumRecordSetsAccumulated(rSetAcc1, rSetAcc2)
      )
      .getOrElse(ZeroRecordSetAccumulated)

  private def sumRecordSetsAccumulated(rSetAcc1: RecordSet.Accumulated, rSetAcc2: RecordSet.Accumulated): RecordSet.Accumulated =
    rSetAcc1
      .modify(_.days).using(_ + rSetAcc2.days)
      .modify(_.records).using { records1 =>
        rSetAcc2.records.foldLeft(records1) { case (records1, fund -> record2) =>
          records1.updatedWith(fund) {
            case Some(record1) => Some(sumRecordsAccumulated(record1, record2))
            case None => Some(record2)
          }
        }
      }
      .modify(_.totalYieldRate).using(totalYieldRate => composeRatesIfAny(totalYieldRate ++ rSetAcc2.totalYieldRate))
      .modify(_.totalYieldResult).using(totalYieldResult => sumIfAny(totalYieldResult ++ rSetAcc2.totalYieldResult))
      .modify(_.totalBalanceChange).using(totalBalanceChange => sumIfAny(totalBalanceChange ++ rSetAcc2.totalBalanceChange))
      .modify(_.missingData).using(_ || rSetAcc2.missingData)

  private def sumRecordsAccumulated(record1: Record.Accumulated, record2: Record.Accumulated): Record.Accumulated =
    record1
      .modify(_.days).using(_ + record2.days)
      .modify(_.yieldRate).using(yieldRate => composeRatesIfAny(yieldRate ++ record2.yieldRate))
      .modify(_.yieldResult).using(yieldResult => sumIfAny(yieldResult ++ record2.yieldResult))
      .modify(_.shareAmountChange).using(shareAmountChange => sumIfAny(shareAmountChange ++ record2.shareAmountChange))
      .modify(_.balanceChange).using(balanceChange => sumIfAny(balanceChange ++ record2.balanceChange))
      .modify(_.missingData).using(_ || record2.missingData)
}
