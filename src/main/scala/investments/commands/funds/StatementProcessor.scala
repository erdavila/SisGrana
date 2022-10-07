package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.{LocalDate, YearMonth}
import utils.Traversing._

object StatementProcessor {
  def process(yearMonth: YearMonth, statement: FundsStatement): (InitialRecordSet, Seq[RecordSet]) = {
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

    val (_, remainingDaysRecords) = statement.entries
      .toSeq.sortBy { case (date, _) => date }
      .foldFlatMapLeft(initialRecordSet: PreviousRecordSet) { case (previousRecordSet, (date, entries)) =>
        val recordSet = recordSetFrom(entries, date, previousRecordSet)
        (recordSet, Some(recordSet))
      }

    (initialRecordSet, remainingDaysRecords)
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
    val records = recordsFrom(dayEntries, days, previousRecordSet)
    recordSetFrom(records, date, days, previousRecordSet)
  }

  private def recordsFrom(dayEntries: Map[String, FundsStatement.Entry], days: Int, previousRecordSet: PreviousRecordSet): Map[String, Record] =
    (dayEntries.keys ++ previousRecordSet.records.keys).toSeq
      .distinct
      .map { fund =>
        val entry = dayEntries.get(fund)
        val previousRecord = previousRecordSet.records.get(fund)
        fund -> recordFrom(entry, days, previousRecord)
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
      accumulatedDays = previousRecordSet.accumulatedDays + days,
      accumulatedTotalYieldRate = composeRatesIfAny(previousRecordSet.accumulatedTotalYieldRate ++ totalYieldRate),
      accumulatedTotalYieldResult = sumIfAny(previousRecordSet.accumulatedTotalYieldResult ++ totalYieldResult),
      accumulatedTotalBalanceChange = sumIfAny(previousRecordSet.accumulatedTotalBalanceChange ++ totalBalanceChange),
    )
  }

  private[funds] def recordFrom(entry: Option[FundsStatement.Entry], days: Int, previousRecord: Option[PreviousRecord]) = {
    val previousMissingData = previousRecord.map(_.missingData)
    val previousSharePrice = previousRecord.flatMap(_.sharePrice)
    val previousFinalBalance = previousRecord.flatMap(_.finalBalance)
    val previousShareAmount = previousRecord.flatMap(_.shareAmount)
    val previousAccumulatedDays = previousRecord.map(_.accumulatedDays)
    val previousAccumulatedYieldRate = previousRecord.flatMap(_.accumulatedYieldRate)
    val previousAccumulatedYieldResult = previousRecord.flatMap(_.accumulatedYieldResult)
    val previousAccumulatedShareAmountChange = previousRecord.flatMap(_.accumulatedShareAmountChange)
    val previousAccumulatedBalanceChange = previousRecord.flatMap(_.accumulatedBalanceChange)

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
      accumulatedDays = (previousAccumulatedDays ++ Option.when(yieldRate.isDefined)(days)).sum,
      accumulatedYieldRate = composeRatesIfAny(previousAccumulatedYieldRate ++ yieldRate),
      accumulatedYieldResult = sumIfAny(previousAccumulatedYieldResult ++ yieldResult),
      accumulatedShareAmountChange = sumIfAny(previousAccumulatedShareAmountChange ++ shareAmountChange),
      accumulatedBalanceChange = sumIfAny(previousAccumulatedBalanceChange ++ balanceChange)
    )
  }

  def sumAccumulatedValues(recordSets: Iterable[RecordSet]): RecordSet = {
    val emptyRecordSet = RecordSet(
      LocalDate.MIN, 0, Map.empty, missingData = false,
      None, None, None, None, None, 0, None, None, None,
    )
    recordSets.foldLeft(emptyRecordSet) { (accumulatedRecordSet, recordSet) =>
      accumulateRecordSetValues(accumulatedRecordSet, recordSet)
    }
  }

  private def accumulateRecordSetValues(recordSet1: RecordSet, recordSet2: RecordSet): RecordSet = {
    val accumulatedTotalYieldResult = sumIfAny(recordSet1.accumulatedTotalYieldResult ++ recordSet2.accumulatedTotalYieldResult)
    val accumulatedTotalYieldRate = composeRatesIfAny(recordSet1.accumulatedTotalYieldRate ++ recordSet2.accumulatedTotalYieldRate)
    val accumulatedTotalBalanceChange = sumIfAny(recordSet1.accumulatedTotalBalanceChange ++ recordSet2.accumulatedTotalBalanceChange)

    recordSet1
      .modify(_.accumulatedDays).using(_ + recordSet2.accumulatedDays)
      .modify(_.records).using(records => accumulateRecordsValues(records, recordSet2.records))
      .modify(_.accumulatedTotalYieldResult).setTo(accumulatedTotalYieldResult)
      .modify(_.accumulatedTotalYieldRate).setTo(accumulatedTotalYieldRate)
      .modify(_.accumulatedTotalBalanceChange).setTo(accumulatedTotalBalanceChange)
  }

  private def accumulateRecordsValues(records1: Map[String, Record], records2: Map[String, Record]): Map[String, Record] =
    records1.foldLeft(records2) { case (records, fund -> record) =>
      records.updatedWith(fund) {
        case Some(existingRecord) => Some(accumulateRecordValues(existingRecord, record))
        case None => Some(record)
      }
    }

  private def accumulateRecordValues(record1: Record, record2: Record): Record = {
    val accumulatedYieldRate = composeRatesIfAny(record1.accumulatedYieldRate ++ record2.accumulatedYieldRate)
    val accumulatedYieldResult = sumIfAny(record1.accumulatedYieldResult ++ record2.accumulatedYieldResult)
    val accumulatedShareAmountChange = sumIfAny(record1.accumulatedShareAmountChange ++ record2.accumulatedShareAmountChange)
    val accumulatedBalanceChange = sumIfAny(record1.accumulatedBalanceChange ++ record2.accumulatedBalanceChange)

    record1
      .modify(_.accumulatedDays).using(_ + record2.accumulatedDays)
      .modify(_.accumulatedYieldRate).setTo(accumulatedYieldRate)
      .modify(_.accumulatedYieldResult).setTo(accumulatedYieldResult)
      .modify(_.accumulatedShareAmountChange).setTo(accumulatedShareAmountChange)
      .modify(_.accumulatedBalanceChange).setTo(accumulatedBalanceChange)
      .modify(_.missingData).using(_ || record2.missingData)
  }
}
