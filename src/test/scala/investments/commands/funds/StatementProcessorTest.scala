package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import investments.Rate
import investments.fileTypes.fundsMonthStatement.FundsStatement
import java.time.{LocalDate, Month, YearMonth}

class StatementProcessorTest extends TestBase {
  private val yearMonth = YearMonth.of(2022, Month.JANUARY)

  private case class PositionRecordFromInputs(days: Int = 1, entry: Option[FundsStatement.Entry], previousPositionRecord: Option[Record.Position.Previous]) {
    def applyToMethod(): Option[Presence[Record.Position]] = StatementProcessor.positionRecordFrom(days, entry, previousPositionRecord)
  }

  private case class PositionRecordSetFromInputs(date: LocalDate = yearMonth.atDay(1), days: Int = 1, positionRecords: Map[String, Presence[Record.Position]], previousPositionRecordSet: RecordSet.Position.Previous) {
    def applyToMethod(): RecordSet.Position = StatementProcessor.positionRecordSetFrom(date, days, positionRecords, previousPositionRecordSet)
  }

  private case class AccumulatedRecordFromInputs(positionRecord: Option[Presence[Record.Position]], previousAccumulatedRecord: Option[Record.Accumulated]) {
    def applyToMethod(): Record.Accumulated = StatementProcessor.accumulatedRecordFrom(positionRecord, previousAccumulatedRecord)
  }

  private case class AccumulatedRecordSetFromInputs(accumulatedRecords: Map[String, Record.Accumulated], positionRecordSet: RecordSet.Position, previousAccumulatedRecordSet: RecordSet.Accumulated) {
    def applyToMethod(): RecordSet.Accumulated = StatementProcessor.accumulatedRecordSetFrom(accumulatedRecords, positionRecordSet, previousAccumulatedRecordSet)
  }

  private val present = defined

  test(".positionRecordsFrom()") {
    val entries = Map(
      "entry" -> anyEntry(),
      "entry|previous" -> anyEntry(),
    )
    val previousPositionRecords = Map(
      "entry|previous" -> anyPositionRecord(),
      "previous" -> anyPositionRecord(),
      "previous-with-shareAmount" -> anyPositionRecord().modify(_.shareAmount).setTo(Some(123.4567890)),
    )

    val positionRecords = StatementProcessor.positionRecordsFrom(days = 1, entries, previousPositionRecords)

    positionRecords.keySet should contain theSameElementsAs Set("entry", "entry|previous", "previous-with-shareAmount")
    positionRecords("entry") should be (present)
    positionRecords("entry|previous") should be (present)
    positionRecords("previous-with-shareAmount") should be (Missing)
  }

  test(".positionRecordFrom()") {
    val previousShareAmount = BigDecimal(123.4567)
    val previousRecordWithShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val previousRecordWithoutShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(None)

    val cases = Table(
      "inputs" -> "expectedRecordPresenceOption",

      PositionRecordFromInputs(
        entry = Some(anyEntry()),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(Present(())),

      PositionRecordFromInputs(
        entry = Some(anyEntry()),
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> Some(Present(())),

      PositionRecordFromInputs(
        entry = Some(anyEntry()),
        previousPositionRecord = None,
      ) -> Some(Present(())),

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(Missing),

      PositionRecordFromInputs(
        entry = None,
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedRecordPresenceOption =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.map(_.map(_ => ())) should equal (expectedRecordPresenceOption)
    }
  }

  test(".positionRecordFrom() sharePrice") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)
    val inputs = PositionRecordFromInputs(
      entry = Some(entry),
      previousPositionRecord = Some(anyPositionRecord()),
    )

    val positionRecord = inputs.applyToMethod()

    positionRecord.value.value.sharePrice should equal (entry.sharePrice)
  }

  test(".positionRecordFrom() yieldRate") {
    val days = 3
    val entry = anyEntry().modify(_.sharePrice).setTo(12.34567890)

    val previousRecord = anyPositionRecord().modify(_.sharePrice).setTo(13.34567890)

    val cases = Table(
      "inputs" -> "expectedYieldRate",

      PositionRecordFromInputs(
        days = days,
        entry = Some(entry),
        previousPositionRecord = Some(previousRecord),
      ) -> Some(Rate(entry.sharePrice / previousRecord.sharePrice - 1, days)),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldRate =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.yieldRate should equal (expectedYieldRate)
    }
  }

  test(".positionRecordFrom() yieldResult") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousSharePrice = 1.34567890
    val previousFinalBalance = 123.45
    val previousRecordWithFinalBalance =
      anyPositionRecord()
        .modify(_.sharePrice).setTo(previousSharePrice)
        .modify(_.finalBalance).setTo(Some(previousFinalBalance))
    val previousRecordWithoutFinalBalance =
      anyPositionRecord()
        .modify(_.sharePrice).setTo(previousSharePrice)
        .modify(_.finalBalance).setTo(None)

    val expectedRate = entry.sharePrice / previousSharePrice - 1
    val expectedYieldResult = previousFinalBalance * expectedRate

    val cases = Table(
      "inputs" -> "expectedYieldResult",

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithFinalBalance),
      ) -> Some(expectedYieldResult),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithoutFinalBalance),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedYieldResult =>
      val positionRecord = inputs.applyToMethod()

      expectedYieldResult match {
        case Some(expectedYieldResult) => positionRecord.value.value.yieldResult.value should equal (expectedYieldResult +- 1e-14)
        case None => positionRecord.value.value.yieldResult should be (None)
      }
    }
  }

  test(".positionRecordFrom() initialBalance") {
    val entry = anyEntry().modify(_.sharePrice).setTo(1.23456789)

    val previousShareAmount = BigDecimal(123.4567)
    val previousRecordWithShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val previousRecordWithoutShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(None)

    val cases = Table(
      "inputs" -> "expectedInitialBalance",

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount.toDouble * entry.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entry),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedInitialBalance =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.initialBalance should equal (expectedInitialBalance)
    }
  }

  test(".positionRecordFrom() shareAmountChange") {
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
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.shareAmountChange should equal (expectedShareAmountChange)
    }
  }

  test(".positionRecordFrom() balanceChange") {
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
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.balanceChange should equal (expectedBalanceChange)
    }
  }

  test(".positionRecordFrom() shareAmount") {
    val shareAmountChange = BigDecimal(12.34567890)
    val entryWithShareAmountChange = anyEntry()
      .modify(_.sharePrice).setTo(1.23456789)
      .modify(_.shareAmountChange).setTo(Some(shareAmountChange))
    val entryWithoutShareAmountChange = anyEntry()
      .modify(_.sharePrice).setTo(1.23456789)
      .modify(_.shareAmountChange).setTo(None)

    val previousShareAmount = BigDecimal(123.4567)
    val previousRecordWithShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val previousRecordWithoutShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(None)

    val cases = Table(
      "inputs" -> "expectedShareAmount",

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount + shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = None,
      ) -> Some(shareAmountChange),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> None,

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = None,
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedShareAmount =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.shareAmount should equal (expectedShareAmount)
    }
  }

  test(".positionRecordFrom() finalBalance") {
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

    val previousRecordWithShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(Some(previousShareAmount))
    val previousRecordWithoutShareAmount = anyPositionRecord().modify(_.shareAmount).setTo(None)

    val cases = Table(
      "inputs" -> "expectedFinalBalance",

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some((previousShareAmount + shareAmountChange).toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithoutShareAmountChange),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> Some(previousShareAmount.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryWithShareAmountChange),
        previousPositionRecord = Some(previousRecordWithoutShareAmount),
      ) -> Some(shareAmountChange.toDouble * entryWithShareAmountChange.sharePrice),

      PositionRecordFromInputs(
        entry = Some(entryForFullWithdrawal),
        previousPositionRecord = Some(previousRecordWithShareAmount),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedFinalBalance =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.finalBalance should equal (expectedFinalBalance)
    }
  }

  test(".positionRecordFrom() note") {
    val note = "Note"

    val entryWithNote = anyEntry().modify(_.note).setTo(Some(note))
    val entryWithoutNote = anyEntry().modify(_.note).setTo(None)

    val cases = Table(
      "inputs" -> "expectedNote",

      PositionRecordFromInputs(
        entry = Some(entryWithNote),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> Some(note),

      PositionRecordFromInputs(
        entry = Some(entryWithoutNote),
        previousPositionRecord = Some(anyPositionRecord()),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedNote =>
      val positionRecord = inputs.applyToMethod()

      positionRecord.value.value.note should equal (expectedNote)
    }
  }

  test(".positionRecordSetFrom() missingData") {
    val cases = Table(
      "inputs" -> "expectedMissingData",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord()),
          "B" -> Present(anyPositionRecord()),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> false,

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord()),
          "B" -> Missing,
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> true,
    )

    forAll(cases) { case inputs -> expectedMissingData =>
      val positionRecordSet = inputs.applyToMethod()

      positionRecordSet.missingData should equal (expectedMissingData)
    }
  }

  test(".positionRecordSetFrom() totalYieldRate") {
    val days = 3
    val previousFinalBalance = 122.4567890
    val initialBalance = 123.4567890
    val expectedTotalYieldRate = Rate(initialBalance / previousFinalBalance - 1, days)

    val cases = Table(
      "inputs" -> "expectedTotalYieldRate",

      PositionRecordSetFromInputs(
        days = days,
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))),
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> Some(expectedTotalYieldRate),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(Some(previousFinalBalance))
      ) -> None,

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalance))),
        ),
        previousPositionRecordSet = anyPositionRecordSet().modify(_.totalFinalBalance).setTo(None)
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldRate =>
      val positionRecordSet = inputs.applyToMethod()

      positionRecordSet.totalYieldRate should equal (expectedTotalYieldRate)
    }
  }

  test(".positionRecordSetFrom() totalYieldResult") {
    val yieldResultA = 12.34567890
    val yieldResultB = 123.4567890

    val expectedTotalYieldResult = yieldResultA + yieldResultB

    val cases = Table(
      "inputs" -> "expectedTotalYieldResult",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultA))),
          "B" -> Present(anyPositionRecord().modify(_.yieldResult).setTo(Some(yieldResultB))),
          "C" -> Present(anyPositionRecord().modify(_.yieldResult).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(expectedTotalYieldResult),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.yieldResult).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalYieldResult =>
      val positionRecordSet = inputs.applyToMethod()

      positionRecordSet.totalYieldResult should equal (expectedTotalYieldResult)
    }
  }

  test(".positionRecordSetFrom() totalInitialBalance") {
    val initialBalanceA = 123.4567890
    val initialBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalInitialBalance",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceA))),
          "B" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(Some(initialBalanceB))),
          "C" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(initialBalanceA + initialBalanceB),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(None)),
          "B" -> Present(anyPositionRecord().modify(_.initialBalance).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalInitialBalance =>
      val positionRecordSet = inputs.applyToMethod()

      expectedTotalInitialBalance match {
        case Some(expectedTotalInitialBalance) => positionRecordSet.totalInitialBalance.value should equal (expectedTotalInitialBalance)
        case None => positionRecordSet.totalInitialBalance should be (None)
      }
    }
  }

  test(".positionRecordSetFrom() totalBalanceChange") {
    val balanceChangeA = 123.4567890
    val balanceChangeB = 1234.567890
    val expectedBalanceChange = balanceChangeA + balanceChangeB

    val cases = Table(
      "inputs" -> "expectedTotalBalanceChange",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeA))),
          "B" -> Present(anyPositionRecord().modify(_.balanceChange).setTo(Some(balanceChangeB))),
          "C" -> Present(anyPositionRecord().modify(_.balanceChange).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(expectedBalanceChange),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.balanceChange).setTo(None)),
          "B" -> Present(anyPositionRecord().modify(_.balanceChange).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalBalanceChange =>
      val positionRecordSet = inputs.applyToMethod()

      expectedTotalBalanceChange match {
        case Some(expectedTotalBalanceChange) => positionRecordSet.totalBalanceChange.value should equal(expectedTotalBalanceChange)
        case None => positionRecordSet.totalBalanceChange should be (None)
      }
    }
  }

  test(".positionRecordSetFrom() totalFinalBalance") {
    val finalBalanceA = 123.4567890
    val finalBalanceB = 1234.567890

    val cases = Table(
      "inputs" -> "expectedTotalFinalBalance",

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceA))),
          "B" -> Present(anyPositionRecord().modify(_.finalBalance).setTo(Some(finalBalanceB))),
          "C" -> Present(anyPositionRecord().modify(_.finalBalance).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> Some(finalBalanceA + finalBalanceB),

      PositionRecordSetFromInputs(
        positionRecords = Map(
          "A" -> Present(anyPositionRecord().modify(_.finalBalance).setTo(None)),
          "B" -> Present(anyPositionRecord().modify(_.finalBalance).setTo(None)),
        ),
        previousPositionRecordSet = anyPositionRecordSet(),
      ) -> None,
    )

    forAll(cases) { case inputs -> expectedTotalFinalBalance =>
      val positionRecordSet = inputs.applyToMethod()

      expectedTotalFinalBalance match {
        case Some(expectedTotalFinalBalance) => positionRecordSet.totalFinalBalance.value should equal (expectedTotalFinalBalance)
        case None => positionRecordSet.totalFinalBalance should be (None)
      }
    }
  }

  test(".accumulatedRecordsFrom()") {
    val positionRecords = Map(
      "current-present" -> Present(anyPositionRecord()),
      "current-missing" -> Missing,
      "current-present|previous" -> Present(anyPositionRecord()),
      "current-missing|previous" -> Missing,
    )
    val previousAccumulatedRecords = Map(
      "current-present|previous" -> anyAccumulatedRecord(),
      "current-missing|previous" -> anyAccumulatedRecord(),
      "previous" -> anyAccumulatedRecord(),
    )

    val accumulatedRecords = StatementProcessor.accumulatedRecordsFrom(positionRecords, previousAccumulatedRecords)

    accumulatedRecords.keySet should contain theSameElementsAs Set(
      "current-present",
      "current-missing",
      "current-present|previous",
      "current-missing|previous",
      "previous",
    )
  }

  test(".accumulatedRecordFrom() missingData") {
    val previousAccumulatedRecordWithMissingDataFalse = anyAccumulatedRecord().modify(_.missingData).setTo(false)
    val previousAccumulatedRecordWithMissingDataTrue = anyAccumulatedRecord().modify(_.missingData).setTo(true)

    val cases = Table(
      "inputs" -> "expectedMissingData",

      AccumulatedRecordFromInputs(
        positionRecord = Some(Present(anyPositionRecord())),
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataFalse),
      ) -> false,

      AccumulatedRecordFromInputs(
        positionRecord = Some(Present(anyPositionRecord())),
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataTrue),
      ) -> true,

      AccumulatedRecordFromInputs(
        positionRecord = Some(Missing),
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataFalse),
      ) -> true,

      AccumulatedRecordFromInputs(
        positionRecord = Some(Missing),
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataTrue),
      ) -> true,

      AccumulatedRecordFromInputs(
        positionRecord = None,
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataFalse),
      ) -> false,

      AccumulatedRecordFromInputs(
        positionRecord = None,
        previousAccumulatedRecord = Some(previousAccumulatedRecordWithMissingDataTrue),
      ) -> true,

      AccumulatedRecordFromInputs(
        positionRecord = Some(Present(anyPositionRecord())),
        previousAccumulatedRecord = None,
      ) -> false,

      AccumulatedRecordFromInputs(
        positionRecord = Some(Missing),
        previousAccumulatedRecord = None,
      ) -> true,
    )

    forAll(cases) { case inputs -> expectedMissingData =>
      val accumulatedRecord = inputs.applyToMethod()

      accumulatedRecord.missingData should equal (expectedMissingData)
    }
  }

  test(".accumulatedRecordSetFrom() missingData") {
    val accumulatedRecordsWithoutMissingData = Map("A" -> anyAccumulatedRecord().modify(_.missingData).setTo(false))
    val accumulatedRecordsWithMissingData = Map("A" -> anyAccumulatedRecord().modify(_.missingData).setTo(true))

    val positionRecordSetWithMissingDataFalse = anyPositionRecordSet().modify(_.missingData).setTo(false)
    val positionRecordSetWithMissingDataTrue = anyPositionRecordSet().modify(_.missingData).setTo(true)

    val previousAccumulatedRecordSetWithMissingDataFalse = anyAccumulatedRecordSet().modify(_.missingData).setTo(false)
    val previousAccumulatedRecordSetWithMissingDataTrue = anyAccumulatedRecordSet().modify(_.missingData).setTo(true)

    val cases = Table(
      "inputs" -> "expectedMissingData",

      AccumulatedRecordSetFromInputs(
        accumulatedRecords = accumulatedRecordsWithoutMissingData,
        positionRecordSet = positionRecordSetWithMissingDataFalse,
        previousAccumulatedRecordSet = previousAccumulatedRecordSetWithMissingDataFalse,
      ) -> false,

      AccumulatedRecordSetFromInputs(
        accumulatedRecords = accumulatedRecordsWithMissingData,
        positionRecordSet = positionRecordSetWithMissingDataFalse,
        previousAccumulatedRecordSet = previousAccumulatedRecordSetWithMissingDataFalse,
      ) -> true,

      AccumulatedRecordSetFromInputs(
        accumulatedRecords = accumulatedRecordsWithoutMissingData,
        positionRecordSet = positionRecordSetWithMissingDataTrue,
        previousAccumulatedRecordSet = previousAccumulatedRecordSetWithMissingDataFalse,
      ) -> true,

      AccumulatedRecordSetFromInputs(
        accumulatedRecords = accumulatedRecordsWithoutMissingData,
        positionRecordSet = positionRecordSetWithMissingDataFalse,
        previousAccumulatedRecordSet = previousAccumulatedRecordSetWithMissingDataTrue,
      ) -> true,
    )

    forAll(cases) { case inputs -> expectedMissingData =>
      val accumulatedRecordSet = inputs.applyToMethod()

      accumulatedRecordSet.missingData should equal (expectedMissingData)
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
      sharePrice = 0.0,
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

  private def anyAccumulatedRecord(): Record.Accumulated = Record.Accumulated(
    yieldRate = None,
    yieldResult = None,
    shareAmountChange = None,
    balanceChange = None,
    missingData = false,
  )

  private def anyAccumulatedRecordSet(): RecordSet.Accumulated = RecordSet.Accumulated(
    days = 0,
    records = Map.empty,
    totalYieldRate = None,
    totalYieldResult = None,
    totalBalanceChange = None,
    missingData = false,
  )
}
