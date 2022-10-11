package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement.FundsStatement
import com.softwaremill.quicklens._
import java.time.{LocalDate, Month, YearMonth}

class StatementProcessorTest extends TestBase {
  private val yearMonth = YearMonth.of(2022, Month.JANUARY)

  private case class PositionRecordFromInputs(entry: Option[FundsStatement.Entry], previousPositionRecord: Option[Record.Position.Previous])
  private case class PositionRecordSetFromInputs(positionRecords: Map[String, Record.Position], date: LocalDate = yearMonth.atDay(1), days: Int = 1, previousPositionRecordSet: RecordSet.Position.Previous)

  test(".positionRecordFrom().sharePrice") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val cases = Table(
      "inputs" -> "expectedSharePrice",
      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> Some(entry.sharePrice),
      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedSharePrice =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(previousRecordWithSharePrice),
      ) -> Some(expectedRate),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithoutSharePrice),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(previousRecordWithSharePrice),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldRate =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> Some(expectedYieldResult),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithSharePriceButWithoutBalance),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(previousRecordWithSharePriceAndBalance),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldResult =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount.toDouble * entry.sharePrice),
      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,
      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedInitialBalance =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = None,
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmountChange =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> Some(expectedBalanceChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = None,
      ) -> Some(expectedBalanceChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedBalanceChange =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount + shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmount =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> Some((previousShareAmount + shareAmountChange).toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> Some(previousShareAmount.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(recordWithoutOwnedFigures),
      ) -> Some(shareAmountChange.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousPositionRecord = Some(recordWithOwnedFigures),
      ) -> None,

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedFinalBalance =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
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
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> Some(note),

      PositionRecordFromInputs(
        entry = Some(anyEntry()),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedNote =>
      val positionRecord = StatementProcessor.positionRecordFrom(inputs.entry, inputs.previousPositionRecord)
      positionRecord.note should equal (expectedNote)
    }
  }

  test(".positionRecordSetFrom().totalYieldRate") {
    val previousFinalBalance = 122.4567890
    val initialBalance = 123.4567890
    val expectedTotalYieldRate = initialBalance / previousFinalBalance - 1

    val cases = Table(
      "inputs" -> "expectedTotalYieldRate",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> Some(expectedTotalYieldRate),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(None)
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> None,

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(None)
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldRate =>
      val positionRecordSet = StatementProcessor.positionRecordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousPositionRecordSet)
      positionRecordSet.totalYieldRate should equal (expectedTotalYieldRate)
    }
  }

  test(".positionRecordSetFrom().totalYieldResult") {
    val yieldResultA = 12.34567890
    val yieldResultB = 123.4567890

    val expectedTotalYieldResult = yieldResultA + yieldResultB

    val cases = Table(
      "inputs" -> "expectedTotalYieldResult",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultA)),
          "B" -> anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultB)),
          "C" -> anyPositionRecord().modify(_.yieldResult).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(expectedTotalYieldResult),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.yieldResult).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldResult =>
      val positionRecordSet = StatementProcessor.positionRecordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousPositionRecordSet)
      positionRecordSet.totalYieldResult should equal (expectedTotalYieldResult)
    }
  }

  test(".positionRecordSetFrom().totalInitialBalance") {
    val initialBalanceA = 123.4567890
    val initialBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalInitialBalance",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceA)),
          "B" -> anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceB)),
          "C" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(initialBalanceA + initialBalanceB),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
          "B" -> anyPositionRecord().modify(_.initialBalance).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalInitialBalance =>
      val positionRecordSet = StatementProcessor.positionRecordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousPositionRecordSet)
      expectedTotalInitialBalance match {
        case Some(expectedTotalInitialBalance) => positionRecordSet.totalInitialBalance.value should equal (expectedTotalInitialBalance)
        case None => positionRecordSet.totalInitialBalance should be (None)
      }
    }
  }

  test(".positionRecordSetFrom().totalBalanceChange") {
    val balanceChangeA = 123.4567890
    val balanceChangeB = 1234.567890
    val expectedBalanceChange = balanceChangeA + balanceChangeB

    val cases = Table(
      "inputs" -> "expectedTotalBalanceChange",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeA)),
          "B" -> anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeB)),
          "C" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(expectedBalanceChange),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
          "B" -> anyPositionRecord().modify(_.balanceChange).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalBalanceChange =>
      val positionRecordSet = StatementProcessor.positionRecordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousPositionRecordSet)
      expectedTotalBalanceChange match {
        case Some(expectedTotalBalanceChange) => positionRecordSet.totalBalanceChange.value should equal(expectedTotalBalanceChange)
        case None => positionRecordSet.totalBalanceChange should be (None)
      }
    }
  }

  test(".positionRecordSetFrom().totalFinalBalance") {
    val finalBalanceA = 123.4567890
    val finalBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalFinalBalance",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceA)),
          "B" -> anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceB)),
          "C" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(finalBalanceA + finalBalanceB),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
          "B" -> anyPositionRecord().modify(_.finalBalance).setTo(None),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalFinalBalance =>
      val positionRecordSet = StatementProcessor.positionRecordSetFrom(inputs.positionRecords, inputs.date, inputs.days, inputs.previousPositionRecordSet)
      expectedTotalFinalBalance match {
        case Some(expectedTotalFinalBalance) => positionRecordSet.totalFinalBalance.value should equal (expectedTotalFinalBalance)
        case None => positionRecordSet.totalFinalBalance should be (None)
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

  private def anyPositionRecordSet(): RecordSet.Position = RecordSet.Position(
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
