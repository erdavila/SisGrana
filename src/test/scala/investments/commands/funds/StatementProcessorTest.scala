package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement.FundsStatement
import com.softwaremill.quicklens._
import java.time.{LocalDate, Month, YearMonth}

class StatementProcessorTest extends TestBase {
  private val yearMonth = YearMonth.of(2022, Month.JANUARY)

  private case class PositionRecordFromInputs(entry: Option[FundsStatement.Entry], previousRecord: Option[PreviousRecord])
  private case class RecordSetFromInputs(positionRecords: Map[String, Record.Position], date: LocalDate = yearMonth.atDay(1), days: Int = 1, previousRecordSet: PreviousRecordSet)

  test(".positionRecordFrom().sharePrice") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val cases = Table(
      "inputs" -> "expectedSharePrice",
      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(anyPositionRecord()),
      ) -> Some(entry.sharePrice),
      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedSharePrice =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.sharePrice should equal (expectedSharePrice)
    }
  }

  test(".positionRecordFrom().yieldRate") {
    val entry = anyEntry().modify(_.sharePrice).setTo(12.34567890)

    val previousSharePrice = 13.34567890
    val previousRecordWithSharePrice = anyPositionRecord().modify(_.sharePrice).setTo(Some(previousSharePrice))
    val previousRecordWithoutSharePrice = anyPositionRecord().modify(_.sharePrice).setTo(None)

    val expectedRate = entry.sharePrice / previousSharePrice - 1

    val cases = Table(
      "inputs" -> "expectedYieldRate",

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePrice),
      ) -> Some(expectedRate),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithoutSharePrice),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithSharePrice),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldRate =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.yieldRate should equal (expectedYieldRate)
    }
  }

  test(".positionRecordFrom().yieldResult") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousSharePrice = 1.34567890
    val previousFinalBalance = 123.45
    val previousRecordWithSharePriceAndBalance =
      anyPositionRecord()
        .modify(_.sharePrice).setTo(Some(previousSharePrice))
        .modify(_.finalBalance).setTo(Some(previousFinalBalance))
    val previousRecordWithSharePriceButWithoutBalance =
      anyPositionRecord()
        .modify(_.sharePrice).setTo(Some(previousSharePrice))
        .modify(_.finalBalance).setTo(None)

    val expectedRate = entry.sharePrice / previousSharePrice - 1
    val expectedYieldResult = previousFinalBalance * expectedRate

    val cases = Table(
      "inputs" -> "expectedYieldResult",

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> Some(expectedYieldResult),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithSharePriceButWithoutBalance),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldResult =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      expectedYieldResult match {
        case Some(expectedYieldResult) => positionRecord.yieldResult.value should equal (expectedYieldResult +- 1e-14)
        case None => positionRecord.yieldResult should be (None)
      }
    }
  }

  test(".positionRecordFrom().initialBalance") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousShareAmount = BigDecimal(123.4567)
    val previousRecordWithShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))

    val cases = Table(
      "inputs" -> "expectedInitialBalance",
      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount.toDouble * entry.sharePrice),
      PositionRecordFromInputs(
        entry = Some(entry),
        previousRecord = None,
      ) -> None,
      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(previousRecordWithShareAmount),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedInitialBalance =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.initialBalance should equal (expectedInitialBalance)
    }
  }

  test(".positionRecordFrom().shareAmountChange") {
    val shareAmountChange = BigDecimal(123.456789)

    val entryWithShareAmountChange = anyEntry().modify(_.shareAmountChange).setTo(Some(shareAmountChange))
    val entryWithoutShareAmountChange = anyEntry().modify(_.shareAmountChange).setTo(None)

    val cases = Table(
      "inputs" -> "expectedShareAmountChange",

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(anyPositionRecord()),
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(anyPositionRecord()),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = None,
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmountChange =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.shareAmountChange should equal (expectedShareAmountChange)
    }
  }

  test(".positionRecordFrom().balanceChange") {
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

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(anyPositionRecord()),
      ) -> Some(expectedBalanceChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(anyPositionRecord()),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = None,
      ) -> Some(expectedBalanceChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedBalanceChange =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.balanceChange should equal (expectedBalanceChange)
    }
  }

  test(".positionRecordFrom().shareAmount") {
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

    val recordWithOwnedFigures = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val recordWithoutOwnedFigures = anyPositionRecord()

    val cases = Table(
      "inputs" -> "expectedShareAmount",

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount + shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmount =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.shareAmount should equal(expectedShareAmount)
    }
  }

  test(".positionRecordFrom().finalBalance") {
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

    val recordWithOwnedFigures = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val recordWithoutOwnedFigures = anyPositionRecord()

    val cases = Table(
      "inputs" -> "expectedFinalBalance",

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some((previousShareAmount + shareAmountChange).toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousRecord = Some(recordWithOwnedFigures),
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedFinalBalance =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.finalBalance should equal (expectedFinalBalance)
    }
  }

  test(".positionRecordFrom().note") {
    val note = "Note"

    val entryWithNote = anyEntry()
      .modify(_.note).setTo(Some(note))

    val cases = Table(
      "inputs" -> "expectedNote",

      PositionRecordFromInputs(
        entry = Some(entryWithNote),
        previousRecord = Some(anyPositionRecord()),
      ) -> Some(note),

      PositionRecordFromInputs(
        entry = Some(anyEntry()),
        previousRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedNote =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousRecord)
      positionRecord.note should equal (expectedNote)
    }
  }

  test(".recordSetFrom().totalYieldRate") {
    val previousFinalBalance = 122.4567890
    val initialBalance = 123.4567890
    val expectedTotalYieldRate = initialBalance / previousFinalBalance - 1

    val cases = Table(
      "inputs" -> "expectedTotalYieldRate",

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> Some(expectedTotalYieldRate),

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(None)
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> None,

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousRecordSet = anyRecordSet().modify(_.totalFinalBalance).setTo(None)
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldRate =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousRecordSet)
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
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultA)),
          "B" -> anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultB)),
          "C" -> anyPositionRecord().modify(_.yieldResult).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(expectedTotalYieldResult),

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.yieldResult).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldResult =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousRecordSet)
      recordSet.totalYieldResult should equal (expectedTotalYieldResult)
    }
  }

  test(".recordSetFrom().totalInitialBalance") {
    val initialBalanceA = 123.4567890
    val initialBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalInitialBalance",

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceA)),
          "B" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceB)),
          "C" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(initialBalanceA + initialBalanceB),

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
          "B" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalInitialBalance =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousRecordSet)
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
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeA)),
          "B" -> anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeB)),
          "C" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(expectedBalanceChange),

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
          "B" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalBalanceChange =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousRecordSet)
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
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceA)),
          "B" -> anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceB)),
          "C" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> Some(finalBalanceA + finalBalanceB),

      RecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
          "B" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
        ),
        previousRecordSet = anyRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalFinalBalance =>
      val recordSet = StatementProcessor.recordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousRecordSet)
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

  private def anyPositionRecord(): Record.Position =
    Record.Position(
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
    positionRecords = Map.empty,
    missingData = false,
    totalYieldRate = None,
    totalYieldResult = None,
    totalInitialBalance = None,
    totalBalanceChange = None,
    totalFinalBalance = None,
  )
}
