package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement.FundsStatement
import com.softwaremill.quicklens._
import java.time.{LocalDate, Month, YearMonth}

class StatementProcessorTest extends TestBase {
  private val yearMonth = YearMonth.of(2022, Month.JANUARY)

  private case class RecordFromInputs(entry: Option[FundsStatement.Entry], previousRecord: Option[PreviousRecord])
  private case class RecordSetFromInputs(records: Map[String, Record], date: LocalDate = yearMonth.atDay(1), days: Int = 1, previousRecordSet: PreviousRecordSet)

  test(".recordFrom().sharePrice") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val cases = Table(
      "inputs" -> "expectedSharePrice",
      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(anyRecord()),
      ) -> Some(entry.sharePrice),
      RecordFromInputs(
        entry = None,
        previousRecord = Some(anyRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedSharePrice =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.sharePrice should equal (expectedSharePrice)
    }
  }

  test(".recordFrom().yieldRate") {
    val entry = anyEntry().modify(_.sharePrice).setTo(12.34567890)

    val previousSharePrice = 13.34567890
    val previousRecordWithSharePrice = anyRecord().modify(_.sharePrice).setTo(Some(previousSharePrice))
    val previousRecordWithoutSharePrice = anyRecord().modify(_.sharePrice).setTo(None)

    val expectedRate = entry.sharePrice / previousSharePrice - 1

    val cases = Table(
      "inputs" -> "expectedYieldRate",

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePrice),
      ) -> Some(expectedRate),

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithoutSharePrice),
      ) -> None,

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,

      RecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithSharePrice),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldRate =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.yieldRate should equal (expectedYieldRate)
    }
  }

  test(".recordFrom().yieldResult") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousSharePrice = 1.34567890
    val previousFinalBalance = 123.45
    val previousRecordWithSharePriceAndBalance =
      anyRecord()
        .modify(_.sharePrice).setTo(Some(previousSharePrice))
        .modify(_.finalBalance).setTo(Some(previousFinalBalance))
    val previousRecordWithSharePriceButWithoutBalance =
      anyRecord()
        .modify(_.sharePrice).setTo(Some(previousSharePrice))
        .modify(_.finalBalance).setTo(None)

    val expectedRate = entry.sharePrice / previousSharePrice - 1
    val expectedYieldResult = previousFinalBalance * expectedRate

    val cases = Table(
      "inputs" -> "expectedYieldResult",

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> Some(expectedYieldResult),

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePriceButWithoutBalance),
      ) -> None,

      RecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,

      RecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldResult =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      expectedYieldResult match {
        case Some(expectedYieldResult) => record.yieldResult.value should equal (expectedYieldResult +- 1e-14)
        case None => record.yieldResult should be (None)
      }
    }
  }

  test(".recordFrom().initialBalance") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousShareAmount = BigDecimal(123.4567)
    val previousRecordWithShareAmount = anyRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))

    val cases = Table(
      "inputs" -> "expectedInitialBalance",
      RecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount.toDouble * entry.sharePrice),
      RecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,
      RecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithShareAmount),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedInitialBalance =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.initialBalance should equal (expectedInitialBalance)
    }
  }

  test(".recordFrom().shareAmountChange") {
    val shareAmountChange = BigDecimal(123.456789)

    val entryWithShareAmountChange = anyEntry().modify(_.shareAmountChange).setTo(Some(shareAmountChange))
    val entryWithoutShareAmountChange = anyEntry().modify(_.shareAmountChange).setTo(None)

    val cases = Table(
      "inputs" -> "expectedShareAmountChange",

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(anyRecord()),
      ) -> Some(shareAmountChange),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(anyRecord()),
      ) -> None,

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = None,
      ) -> Some(shareAmountChange),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmountChange =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.shareAmountChange should equal (expectedShareAmountChange)
    }
  }

  test(".recordFrom().balanceChange") {
    val sharePrice: Double = 1.23456789
    val shareAmount = BigDecimal(123.456789)

    val entryWithShareAmountChange = anyEntry()
      .modify(_.sharePrice).setTo(sharePrice)
      .modify(_.shareAmountChange).setTo(Some(shareAmount))
    val entryWithoutShareAmountChange = anyEntry()
      .modify(_.sharePrice).setTo(sharePrice)
      .modify(_.shareAmountChange).setTo(None)

    val expectedBalanceChange = shareAmount.toDouble * entryWithShareAmountChange.sharePrice

    val cases = Table(
      "inputs" -> "expectedBalanceChange",

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(anyRecord()),
      ) -> Some(expectedBalanceChange),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(anyRecord()),
      ) -> None,

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = None,
      ) -> Some(expectedBalanceChange),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedBalanceChange =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.balanceChange should equal (expectedBalanceChange)
    }
  }

  test(".recordFrom().shareAmount") {
    val shareAmountChange = BigDecimal(123)
    val previousShareAmount = BigDecimal(321)

    val entryWithShareAmountChange =
      anyEntry()
        .modify(_.shareAmountChange).setTo(Some(shareAmountChange))
    val entryWithoutShareAmountChange =
      anyEntry()
        .modify(_.shareAmountChange).setTo(None)
    val entryForFullWithdrawal =
      anyEntry()
        .modify(_.shareAmountChange).setTo(Some(-previousShareAmount))

    val recordWithOwnedFigures = anyRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val recordWithoutOwnedFigures = anyRecord()

    val cases = Table(
      "inputs" -> "expectedShareAmount",

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount + shareAmountChange),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount),

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange),

      RecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> None,

      RecordFromInputs(
        entry = None,
        previousRecord = Some(anyRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmount =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.shareAmount should equal(expectedShareAmount)
    }
  }

  test(".recordFrom().finalBalance") {
    val shareAmountChange = BigDecimal(123)
    val previousShareAmount = BigDecimal(321)

    val entryWithShareAmountChange =
      anyEntry()
        .modify(_.sharePrice).setTo(1.23456789)
        .modify(_.shareAmountChange).setTo(Some(shareAmountChange))
    val entryWithoutShareAmountChange = anyEntry().modify(_.sharePrice).setTo(1.23456789)
    val entryForFullWithdrawal =
      anyEntry()
        .modify(_.sharePrice).setTo(1.23456789)
        .modify(_.shareAmountChange).setTo(Some(-previousShareAmount))

    val recordWithOwnedFigures = anyRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val recordWithoutOwnedFigures = anyRecord()

    val cases = Table(
      "inputs" -> "expectedFinalBalance",

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some((previousShareAmount + shareAmountChange).toDouble * entryWithShareAmountChange.sharePrice),

      RecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount.toDouble * entryWithShareAmountChange.sharePrice),

      RecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange.toDouble * entryWithShareAmountChange.sharePrice),

      RecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> None,

      RecordFromInputs(
        entry = None,
        previousRecord = Some(anyRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedFinalBalance =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.finalBalance should equal (expectedFinalBalance)
    }
  }

  test(".recordFrom().note") {
    val note = "Note"

    val entryWithNote = anyEntry()
      .modify(_.note).setTo(Some(note))

    val cases = Table(
      "inputs" -> "expectedNote",

      RecordFromInputs(
        entry = Some(entryWithNote),
        previousRecord = Some(anyRecord()),
      ) -> Some(note),

      RecordFromInputs(
        entry = Some(anyEntry()),
        previousRecord = Some(anyRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedNote =>
      val record = StatementProcessor.recordFrom(inputs.entry, inputs.previousRecord)
      record.note should equal (expectedNote)
    }
  }

  test(".recordSetFrom().totalYieldRate") {
    val previousFinalBalance = 122.4567890
    val initialBalance = 123.4567890
    val expectedTotalYieldRate = initialBalance / previousFinalBalance - 1

    val cases = Table(
      "inputs" -> "expectedTotalYieldRate",

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> Some(expectedTotalYieldRate),

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.initialBalance).setTo(None)
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> None,

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(None)
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldRate =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.records, inputs.date, inputs.days, inputs.previousRecordSet)
      recordSet.totalYieldRate should equal (expectedTotalYieldRate)
    }
  }

  test(".recordSetFrom().totalYieldResult") {
    val yieldResultA = 12.34567890
    val yieldResultB = 123.4567890

    val expectedTotalYieldResult = yieldResultA + yieldResultB

    val cases = Table(
      "inputs" -> "expectedTotalYieldResult",

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.yieldResult).setTo(Some(yieldResultA)),
          "B" -> anyRecord().modify(_.yieldResult).setTo(Some(yieldResultB)),
          "C" -> anyRecord().modify(_.yieldResult).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(expectedTotalYieldResult),

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.yieldResult).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldResult =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.records, inputs.date, inputs.days, inputs.previousRecordSet)
      recordSet.totalYieldResult should equal (expectedTotalYieldResult)
    }
  }

  test(".recordSetFrom().totalInitialBalance") {
    val initialBalanceA = 123.4567890
    val initialBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalInitialBalance",

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.initialBalance).setTo(Some(initialBalanceA)),
          "B" -> anyRecord().modify(_.initialBalance).setTo(Some(initialBalanceB)),
          "C" -> anyRecord().modify(_.initialBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(initialBalanceA + initialBalanceB),

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.initialBalance).setTo(None),
          "B" -> anyRecord().modify(_.initialBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalInitialBalance =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.records, inputs.date, inputs.days, inputs.previousRecordSet)
      expectedTotalInitialBalance match {
        case Some(expectedTotalInitialBalance) => recordSet.totalInitialBalance.value should equal (expectedTotalInitialBalance)
        case None => recordSet.totalInitialBalance should be (None)
      }
    }
  }

  test(".recordSetFrom().totalBalanceChange") {
    val balanceChangeA = 123.4567890
    val balanceChangeB = 1234.567890
    val expectedBalanceChange = balanceChangeA + balanceChangeB

    val cases = Table(
      "inputs" -> "expectedTotalBalanceChange",

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.balanceChange).setTo(Some(balanceChangeA)),
          "B" -> anyRecord().modify(_.balanceChange).setTo(Some(balanceChangeB)),
          "C" -> anyRecord().modify(_.balanceChange).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(expectedBalanceChange),

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.balanceChange).setTo(None),
          "B" -> anyRecord().modify(_.balanceChange).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalBalanceChange =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.records, inputs.date, inputs.days, inputs.previousRecordSet)
      expectedTotalBalanceChange match {
        case Some(expectedTotalBalanceChange) => recordSet.totalBalanceChange.value should equal(expectedTotalBalanceChange)
        case None => recordSet.totalBalanceChange should be (None)
      }
    }
  }

  test(".recordSetFrom().totalFinalBalance") {
    val finalBalanceA = 123.4567890
    val finalBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalFinalBalance",

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.finalBalance).setTo(Some(finalBalanceA)),
          "B" -> anyRecord().modify(_.finalBalance).setTo(Some(finalBalanceB)),
          "C" -> anyRecord().modify(_.finalBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(finalBalanceA + finalBalanceB),

      RecordSetFromInputs(
        records = Map(
          "A" -> anyRecord().modify(_.finalBalance).setTo(None),
          "B" -> anyRecord().modify(_.finalBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalFinalBalance =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.records, inputs.date, inputs.days, inputs.previousRecordSet)
      expectedTotalFinalBalance match {
        case Some(expectedTotalFinalBalance) => recordSet.totalFinalBalance.value should equal (expectedTotalFinalBalance)
        case None => recordSet.totalFinalBalance should be (None)
      }
    }
  }

  private def anyEntry(): FundsStatement.Entry =
    FundsStatement.Entry(
      sharePrice = math.random(),
      shareAmountChange = None,
      note = None,
    )

  private def anyRecord(): Record =
    Record(
      missingData = false,
      sharePrice = None,
      yieldRate = None,
      yieldResult = None,
      initialBalance = None,
      shareAmountChange = None,
      balanceChange = None,
      shareAmount = None,
      finalBalance = None,
      note = None,
    )

  private def anyRecordSet(): RecordSet = RecordSet(
    date = yearMonth.atDay(1),
    days = 0,
    records = Map.empty,
    missingData = false,
    totalYieldRate = None,
    totalYieldResult = None,
    totalInitialBalance = None,
    totalBalanceChange = None,
    totalFinalBalance = None,
  )
}
