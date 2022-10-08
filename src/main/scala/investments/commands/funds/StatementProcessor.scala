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

  def process(yearMonth: YearMonth, statement: FundsStatement): (InitialRecordSet, Seq[RecordSet], RecordSet.Accumulated) = {
    val initialRecords = statement.initialEntries
      .view.mapValues(initialRecordFrom)
      .toMap

    val initialDate = yearMonth.atDay(1).minusDays(1)
    val initialRecordSet = InitialRecordSet(
      date = initialDate,
      records = initialRecords,
      totalFinalBalance = sumIfAny(initialRecords.values.flatMap(_.finalBalance))
    )
    implicit val daysCounter: DaysCounter = new DaysCounter(statement.noPriceDates)

    val ((_, recordSetAccumulated), remainingDaysRecords) = statement.entries
      .toSeq.sortBy { case (date, _) => date }
      .foldFlatMapLeft((initialRecordSet: PreviousRecordSet, ZeroRecordSetAccumulated)) { case ((previousRecordSet, recordSetAccumulated), (date, entries)) =>
        val recordSet = recordSetFrom(entries, date, previousRecordSet)
        val updatedRecordSetAccumulated = recordSetAccumulatedFrom(recordSetAccumulated, recordSet)
        ((recordSet, updatedRecordSetAccumulated), Some(recordSet))
      }

    (initialRecordSet, remainingDaysRecords, recordSetAccumulated)
  }

  private def initialRecordFrom(initialEntry: FundsStatement.InitialEntry): InitialRecord =
    InitialRecord(
      sharePrice = Some(initialEntry.sharePrice),
      shareAmount = Some(initialEntry.shareAmount),
      finalBalance = Some(initialEntry.shareAmount.toDouble * initialEntry.sharePrice),
      note = initialEntry.note,
    )

  private def recordSetFrom(dayEntries: Map[String, FundsStatement.Entry], date: LocalDate, previousRecordSet: PreviousRecordSet)(implicit daysCounter: DaysCounter): RecordSet = {
    val days = daysCounter.count(previousRecordSet.date, date)
    val records = recordsFrom(dayEntries, previousRecordSet)
    recordSetFrom(records, date, days, previousRecordSet)
  }

  private def recordsFrom(dayEntries: Map[String, FundsStatement.Entry], previousRecordSet: PreviousRecordSet): Map[String, Record] =
    (dayEntries.keys ++ previousRecordSet.records.keys).toSeq
      .distinct
      .map { fund =>
        val entry = dayEntries.get(fund)
        val previousRecord = previousRecordSet.records.get(fund)
        fund -> recordFrom(entry, previousRecord)
      }
      .toMap

  private[funds] def recordSetFrom(records: Map[String, Record], date: LocalDate, days: Int, previousRecordSet: PreviousRecordSet): RecordSet = {
    val totalInitialBalance = sumIfAny(records.values.flatMap(_.initialBalance))
    val totalYieldRate = (totalInitialBalance, previousRecordSet.totalFinalBalance).mapN(_ / _ - 1)
    val totalYieldResult = sumIfAny(records.values.flatMap(_.yieldResult))
    val totalBalanceChange = sumIfAny(records.values.flatMap(_.balanceChange))

    RecordSet(
      date = date,
      days = days,
      records = records,
      missingData = previousRecordSet.missingData || records.values.exists(_.missingData),
      totalYieldRate = totalYieldRate,
      totalYieldResult = totalYieldResult,
      totalInitialBalance = totalInitialBalance,
      totalBalanceChange = totalBalanceChange,
      totalFinalBalance = sumIfAny(records.values.flatMap(_.finalBalance)),
    )
  }

  private[funds] def recordFrom(entry: Option[FundsStatement.Entry], previousRecord: Option[PreviousRecord]) = {
    val previousMissingData = previousRecord.map(_.missingData)
    val previousSharePrice = previousRecord.flatMap(_.sharePrice)
    val previousFinalBalance = previousRecord.flatMap(_.finalBalance)
    val previousShareAmount = previousRecord.flatMap(_.shareAmount)

    val sharePrice = entry.map(_.sharePrice)
    val yieldRate = (sharePrice, previousSharePrice).mapN(_ / _ - 1)
    val yieldResult = (previousFinalBalance, yieldRate).mapN(_ * _)
    val shareAmountChange = entry.flatMap(_.shareAmountChange)
    val balanceChange = (shareAmountChange, sharePrice).mapN(_.toDouble * _)
    val shareAmount = Some((previousShareAmount ++ shareAmountChange).sum).filter(_ != 0)

    Record(
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

  private def recordSetAccumulatedFrom(previousRecordSetAccumulated: RecordSet.Accumulated, recordSet: RecordSet): RecordSet.Accumulated = {
    val currentRecordSetAccumulated = toRecordSetAccumulated(recordSet)
    sumRecordSetsAccumulated(previousRecordSetAccumulated, currentRecordSetAccumulated)
  }

  private def toRecordSetAccumulated(recordSet: RecordSet): RecordSet.Accumulated =
    RecordSet.Accumulated(
      days = recordSet.days,
      records = recordSet.records
        .view
        .mapValues(record =>
          toRecordAccumulated(record, recordSet.days)
        )
        .toMap,
      totalYieldRate = recordSet.totalYieldRate,
      totalYieldResult = recordSet.totalYieldResult,
      totalBalanceChange = recordSet.totalBalanceChange,
      missingData = recordSet.missingData,
    )

  private def toRecordAccumulated(record: Record, days: Int): Record.Accumulated =
    Record.Accumulated(
      days = record.yieldRate.fold(0)(_ => days),
      yieldRate = record.yieldRate,
      yieldResult = record.yieldResult,
      shareAmountChange = record.shareAmountChange,
      balanceChange = record.balanceChange,
      missingData = record.missingData,
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
