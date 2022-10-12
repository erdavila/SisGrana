package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.{LocalDate, YearMonth}
import utils.Traversing._

object StatementProcessor {
  private val ZeroAccumulatedRecordSet = RecordSet.Accumulated(
    days = 0,
    records = Map.empty,
    totalYieldRate = None,
    totalYieldResult = None,
    totalBalanceChange = None,
    missingData = false,
  )

  def process(yearMonth: YearMonth, statement: FundsStatement): (RecordSet.Position.Initial, Seq[RecordSet]) = {
    val initialRecords = statement.initialEntries
      .view.mapValues(initialPositionRecordFrom)
      .toMap
    val initialPositionRecordSet = initialPositionRecordSetFrom(initialRecords, yearMonth)

    implicit val daysCounter: DaysCounter = new DaysCounter(statement.noPriceDates)

    val (_, remainingDaysPositionRecords) = statement.entries
      .toSeq.sortBy { case (date, _) => date }
      .foldFlatMapLeft((initialPositionRecordSet: RecordSet.Position.Previous, ZeroAccumulatedRecordSet)) { case ((previousPositionRecordSet, accumulatedRecordSet), (date, entries)) =>
        val positionRecordSet = positionRecordSetFrom(entries, date, previousPositionRecordSet)
        val updatedAccumulatedRecordSet = accumulatedRecordSetFrom(accumulatedRecordSet, positionRecordSet)
        val recordSet = RecordSet(positionRecordSet, updatedAccumulatedRecordSet)
        ((positionRecordSet, updatedAccumulatedRecordSet), Some(recordSet))
      }

    (initialPositionRecordSet, remainingDaysPositionRecords)
  }

  private def initialPositionRecordFrom(initialEntry: FundsStatement.InitialEntry): Record.Position.Initial =
    Record.Position.Initial(
      sharePrice = Some(initialEntry.sharePrice),
      shareAmount = Some(initialEntry.shareAmount),
      finalBalance = Some(initialEntry.shareAmount.toDouble * initialEntry.sharePrice),
      note = initialEntry.note,
    )

  private def initialPositionRecordSetFrom(
    initialPositionRecords: Map[String, Record.Position.Initial],
    yearMonth: YearMonth,
  ): RecordSet.Position.Initial = {
    val initialDate = yearMonth.atDay(1).minusDays(1)
    RecordSet.Position.Initial(
      date = initialDate,
      positionRecords = initialPositionRecords,
      totalFinalBalance = sumIfAny(initialPositionRecords.values.flatMap(_.finalBalance))
    )
  }

  private def positionRecordSetFrom(dayEntries: Map[String, FundsStatement.Entry], date: LocalDate, previousPositionRecordSet: RecordSet.Position.Previous)(implicit daysCounter: DaysCounter): RecordSet.Position = {
    val days = daysCounter.count(previousPositionRecordSet.date, date)
    val positionRecords = positionRecordsFrom(dayEntries, previousPositionRecordSet)
    positionRecordSetFrom(positionRecords, date, days, previousPositionRecordSet)
  }

  private def positionRecordsFrom(dayEntries: Map[String, FundsStatement.Entry], previousPositionRecordSet: RecordSet.Position.Previous): Map[String, Record.Position] =
    (dayEntries.keys ++ previousPositionRecordSet.positionRecords.keys).toSeq
      .distinct
      .map { fund =>
        val entry = dayEntries.get(fund)
        val previousPositionRecord = previousPositionRecordSet.positionRecords.get(fund)
        fund -> positionRecordFrom(entry, previousPositionRecord)
      }
      .toMap

  private[funds] def positionRecordSetFrom(positionRecords: Map[String, Record.Position], date: LocalDate, days: Int, previousPositionRecordSet: RecordSet.Position.Previous): RecordSet.Position = {
    val totalInitialBalance = sumIfAny(positionRecords.values.flatMap(_.initialBalance))
    val totalYieldRate = (totalInitialBalance, previousPositionRecordSet.totalFinalBalance).mapN(_ / _ - 1)
    val totalYieldResult = sumIfAny(positionRecords.values.flatMap(_.yieldResult))
    val totalBalanceChange = sumIfAny(positionRecords.values.flatMap(_.balanceChange))

    RecordSet.Position(
      date = date,
      days = days,
      positionRecords = positionRecords,
      missingData = previousPositionRecordSet.missingData || positionRecords.values.exists(_.missingData),
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

  private def accumulatedRecordSetFrom(previousAccumulatedRecordSet: RecordSet.Accumulated, positionRecordSet: RecordSet.Position): RecordSet.Accumulated = {
    val currentAccumulatedRecordSet = toAccumulatedRecordSet(positionRecordSet)
    sumAccumulatedRecordSets(previousAccumulatedRecordSet, currentAccumulatedRecordSet)
  }

  private def toAccumulatedRecordSet(positionRecordSet: RecordSet.Position): RecordSet.Accumulated =
    RecordSet.Accumulated(
      days = positionRecordSet.days,
      records = positionRecordSet.positionRecords
        .view
        .mapValues(positionRecord =>
          toAccumulatedRecord(positionRecord, positionRecordSet.days)
        )
        .toMap,
      totalYieldRate = positionRecordSet.totalYieldRate,
      totalYieldResult = positionRecordSet.totalYieldResult,
      totalBalanceChange = positionRecordSet.totalBalanceChange,
      missingData = positionRecordSet.missingData,
    )

  private def toAccumulatedRecord(positionRecord: Record.Position, days: Int): Record.Accumulated =
    Record.Accumulated(
      days = positionRecord.yieldRate.fold(0)(_ => days),
      yieldRate = positionRecord.yieldRate,
      yieldResult = positionRecord.yieldResult,
      shareAmountChange = positionRecord.shareAmountChange,
      balanceChange = positionRecord.balanceChange,
      missingData = positionRecord.missingData,
    )

  def sumAccumulatedRecordSets(accumulatedRecordSets: Iterable[RecordSet.Accumulated]): RecordSet.Accumulated =
    accumulatedRecordSets
      .reduceOption((rSetAcc1, rSetAcc2) =>
        sumAccumulatedRecordSets(rSetAcc1, rSetAcc2)
      )
      .getOrElse(ZeroAccumulatedRecordSet)

  private def sumAccumulatedRecordSets(accRSet1: RecordSet.Accumulated, accRSet2: RecordSet.Accumulated): RecordSet.Accumulated =
    accRSet1
      .modify(_.days).using(_ + accRSet2.days)
      .modify(_.records).using { records1 =>
        accRSet2.records.foldLeft(records1) { case (records1, fund -> record2) =>
          records1.updatedWith(fund) {
            case Some(record1) => Some(sumAccumulatedRecords(record1, record2))
            case None => Some(record2)
          }
        }
      }
      .modify(_.totalYieldRate).using(totalYieldRate => composeRatesIfAny(totalYieldRate ++ accRSet2.totalYieldRate))
      .modify(_.totalYieldResult).using(totalYieldResult => sumIfAny(totalYieldResult ++ accRSet2.totalYieldResult))
      .modify(_.totalBalanceChange).using(totalBalanceChange => sumIfAny(totalBalanceChange ++ accRSet2.totalBalanceChange))
      .modify(_.missingData).using(_ || accRSet2.missingData)

  private def sumAccumulatedRecords(record1: Record.Accumulated, record2: Record.Accumulated): Record.Accumulated =
    record1
      .modify(_.days).using(_ + record2.days)
      .modify(_.yieldRate).using(yieldRate => composeRatesIfAny(yieldRate ++ record2.yieldRate))
      .modify(_.yieldResult).using(yieldResult => sumIfAny(yieldResult ++ record2.yieldResult))
      .modify(_.shareAmountChange).using(shareAmountChange => sumIfAny(shareAmountChange ++ record2.shareAmountChange))
      .modify(_.balanceChange).using(balanceChange => sumIfAny(balanceChange ++ record2.balanceChange))
      .modify(_.missingData).using(_ || record2.missingData)
}
