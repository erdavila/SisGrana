package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement._
import utils.Traversing._
import cats.instances.option._
import cats.syntax.apply._
import java.time.{LocalDate, YearMonth}

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
        val recordSet = recordSetFromX(entries, date, previousRecordSet)
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

  private def recordSetFromX(dayEntries: Map[String, FundsStatement.Entry], date: LocalDate, previousRecordSet: PreviousRecordSet)(implicit daysCounter: DaysCounter): RecordSet = {
    val days = daysCounter.count(previousRecordSet.date, date)
    val records = recordsFrom(dayEntries, previousRecordSet)
    recordSetFromY(records, date, days, previousRecordSet)
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

  private[funds] def recordSetFromY(records: Map[String, Record], date: LocalDate, days: Int, previousRecordSet: PreviousRecordSet): RecordSet = {
    val totalInitialBalance = sumIfAny(records.values.flatMap(_.initialBalance))
    val totalYieldRate = (totalInitialBalance, previousRecordSet.totalFinalBalance).mapN(_ / _ - 1)
    val totalYieldResult = sumIfAny(records.values.flatMap(_.yieldResult))
    val totalBalanceChange = sumIfAny(records.values.flatMap(_.balanceChange))

    RecordSet(
      date = date,
      days = days,
      records = records,
      totalYieldRate = totalYieldRate,
      totalYieldResult = totalYieldResult,
      totalInitialBalance = totalInitialBalance,
      totalBalanceChange = totalBalanceChange,
      totalFinalBalance = sumIfAny(records.values.flatMap(_.finalBalance)),
    )
  }

  private[funds] def recordFrom(entry: Option[FundsStatement.Entry], previousRecord: Option[PreviousRecord]) = {
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
}
