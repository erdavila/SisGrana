package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.Rate
import investments.commands.funds.Record.Position
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

    val daysCounter = new DaysCounter(statement.noPriceDates)

    val (_, recordSets) = statement.entries
      .toSeq.sortBy { case (date, _) => date }
      .foldMapLeft((initialPositionRecordSet: RecordSet.Position.Previous, ZeroAccumulatedRecordSet)) { case ((previousPositionRecordSet, previousAccumulatedRecordSet), (date, entries)) =>
        val days = daysCounter.count(previousPositionRecordSet.date, date)

        val positionRecords = positionRecordsFrom(days, entries, previousPositionRecordSet.positionRecords.present)
        val positionRecordSet = positionRecordSetFrom(date, days, positionRecords, previousPositionRecordSet)

        val accumulatedRecords = accumulatedRecordsFrom(positionRecords, previousAccumulatedRecordSet.records)
        val accumulatedRecordSet = accumulatedRecordSetFrom(accumulatedRecords, positionRecordSet, previousAccumulatedRecordSet)

        val recordSet = RecordSet(positionRecordSet, accumulatedRecordSet)
        ((positionRecordSet, accumulatedRecordSet), recordSet)
      }

    (initialPositionRecordSet, recordSets)
  }

  private def initialPositionRecordFrom(initialEntry: FundsStatement.InitialEntry): Record.Position.Initial =
    Record.Position.Initial(
      sharePrice = initialEntry.sharePrice,
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
      positionRecords = initialPositionRecords.view.mapValues(Present(_)).toMap,
      totalFinalBalance = sumIfAny(initialPositionRecords.values.flatMap(_.finalBalance))
    )
  }

  private[funds] def positionRecordsFrom(
    days: Int,
    entries: Map[String, FundsStatement.Entry],
    previousPositionRecords: Map[String, Record.Position.Previous],
  ): Map[String, Presence[Record.Position]] =
    (entries.keys ++ previousPositionRecords.keys)
      .toSeq
      .distinct
      .flatMap { fund =>
        val entry = entries.get(fund)
        val previousPositionRecord = previousPositionRecords.get(fund)
        positionRecordFrom(days, entry, previousPositionRecord).map(fund -> _)
      }
      .toMap

  private[funds] def positionRecordFrom(
    days: Int,
    entry: Presence[FundsStatement.Entry],
    previousPositionRecord: Presence[Position.Previous],
  ): Option[Presence[Position]] = {
    val previousShareAmount = previousPositionRecord.flatMap(_.shareAmount)
    entry match {
      case Some(entry) =>
        val yieldRate = previousPositionRecord.map(previousPositionRecord => Rate(entry.sharePrice / previousPositionRecord.sharePrice - 1, days))
        val shareAmount = sumIfAny(previousShareAmount ++ entry.shareAmountChange).filter(_ != 0)
        Some(
          Present(
            Record.Position(
              sharePrice = entry.sharePrice,
              yieldRate = yieldRate,
              yieldResult = (previousPositionRecord.flatMap(_.finalBalance), yieldRate).mapN(_ * _.value),
              initialBalance = previousShareAmount.map(_.toDouble * entry.sharePrice),
              shareAmountChange = entry.shareAmountChange,
              balanceChange = entry.shareAmountChange.map(_.toDouble * entry.sharePrice),
              shareAmount = shareAmount,
              finalBalance = shareAmount.map(_.toDouble * entry.sharePrice),
              note = entry.note,
            )
          )
        )
      case None => Option.when(previousShareAmount.isDefined)(Missing)
    }
  }

  private[funds] def positionRecordSetFrom(
    date: LocalDate,
    days: Int,
    positionRecords: Map[String, Presence[Record.Position]],
    previousPositionRecordSet: RecordSet.Position.Previous,
  ): RecordSet.Position = {
    val (fundsMissingData, presentPreviousRecordsMap) = positionRecords.partitionByPresence
    val presentPreviousRecords = presentPreviousRecordsMap.values
    val totalInitialBalance = sumIfAny(presentPreviousRecords.flatMap(_.initialBalance))

    RecordSet.Position(
      date = date,
      days = days,
      positionRecords = positionRecords,
      missingData = fundsMissingData.nonEmpty,
      totalYieldRate = (totalInitialBalance, previousPositionRecordSet.totalFinalBalance).mapN { (totalInitialBalance, previousTotalFinalBalance) =>
        Rate(totalInitialBalance / previousTotalFinalBalance - 1, days)
      },
      totalYieldResult = sumIfAny(presentPreviousRecords.flatMap(_.yieldResult)),
      totalInitialBalance = totalInitialBalance,
      totalBalanceChange = sumIfAny(presentPreviousRecords.flatMap(_.balanceChange)),
      totalFinalBalance = sumIfAny(presentPreviousRecords.flatMap(_.finalBalance)),
    )
  }

  private[funds] def accumulatedRecordsFrom(
    positionRecords: Map[String, Presence[Record.Position]],
    previousAccumulatedRecords: Map[String, Record.Accumulated],
  ): Map[String, Record.Accumulated] =
    (positionRecords.keys ++ previousAccumulatedRecords.keys)
      .toSeq
      .distinct
      .map { fund =>
        val positionRecord = positionRecords.get(fund)
        val previousAccumulatedRecord = previousAccumulatedRecords.get(fund)
        fund -> accumulatedRecordFrom(positionRecord, previousAccumulatedRecord)
      }
      .toMap

  private[funds] def accumulatedRecordFrom(
    positionRecord: Option[Presence[Record.Position]],
    previousAccumulatedRecord: Option[Record.Accumulated],
  ): Record.Accumulated = {
    val presentPositionRecord = positionRecord.flatten

    Record.Accumulated(
      yieldRate = composeRatesIfAny(presentPositionRecord.flatMap(_.yieldRate) ++ previousAccumulatedRecord.flatMap(_.yieldRate)),
      yieldResult = sumIfAny(presentPositionRecord.flatMap(_.yieldResult) ++ previousAccumulatedRecord.flatMap(_.yieldResult)),
      shareAmountChange = sumIfAny(presentPositionRecord.flatMap(_.shareAmountChange) ++ previousAccumulatedRecord.flatMap(_.shareAmountChange)),
      balanceChange = sumIfAny(presentPositionRecord.flatMap(_.balanceChange) ++ previousAccumulatedRecord.flatMap(_.balanceChange)),
      missingData = previousAccumulatedRecord.fold(false)(_.missingData) || positionRecord.contains(Missing),
    )
  }

  private[funds] def accumulatedRecordSetFrom(
    accumulatedRecords: Map[String, Record.Accumulated],
    positionRecordSet: RecordSet.Position,
    previousAccumulatedRecordSet: RecordSet.Accumulated,
  ): RecordSet.Accumulated =
    RecordSet.Accumulated(
      days = positionRecordSet.days + previousAccumulatedRecordSet.days,
      records = accumulatedRecords,
      totalYieldRate = composeRatesIfAny(positionRecordSet.totalYieldRate ++ previousAccumulatedRecordSet.totalYieldRate),
      totalYieldResult = sumIfAny(positionRecordSet.totalYieldResult ++ previousAccumulatedRecordSet.totalYieldResult),
      totalBalanceChange = sumIfAny(positionRecordSet.totalBalanceChange ++ previousAccumulatedRecordSet.totalBalanceChange),
      missingData = positionRecordSet.missingData || accumulatedRecords.values.exists(_.missingData) || previousAccumulatedRecordSet.missingData,
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
      .modify(_.yieldRate).using(yieldRate => composeRatesIfAny(yieldRate ++ record2.yieldRate))
      .modify(_.yieldResult).using(yieldResult => sumIfAny(yieldResult ++ record2.yieldResult))
      .modify(_.shareAmountChange).using(shareAmountChange => sumIfAny(shareAmountChange ++ record2.shareAmountChange))
      .modify(_.balanceChange).using(balanceChange => sumIfAny(balanceChange ++ record2.balanceChange))
      .modify(_.missingData).using(_ || record2.missingData)
}
