package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import java.time.YearMonth
import utils.AnsiString.{Code, StringOps}
import utils.TextAligner.Chunk
import utils.{AnsiString, BrNumber}

class ChunkMaker(options: ChunkMaker.Options) {
  def makeChunks(
    yearMonth: YearMonth,
    warningText: Option[String],
    initialRecordSet: Option[InitialRecordSet],
    recordSets: Seq[RecordSet],
    recordSetAccumulated: RecordSet.Accumulated,
  ): Seq[Seq[Chunk]] = {
    val yearMonthRowChunk = Seq(Chunk.leftAligned(Anchors.Leftmost, yearMonth.toString))

    val warningChunks = warningText
      .toSeq
      .map { text =>
        val warning = "  " ++ toWarningAnsiString(text)
        Seq(Chunk.leftAligned(Anchors.Leftmost, warning.toString, warning.length))
      }

    val daysRowsChunks = seqIf(options.days) {
      val initialRecordSetRowsChunks = initialRecordSet
        .toSeq
        .flatMap(toInitialRecordSetChunks)

      val recordSetsRowsChunks = recordSets.flatMap(toRecordSetChunks)
      initialRecordSetRowsChunks ++ recordSetsRowsChunks
    }

    val monthSummaryRowsChunks = toMonthSummaryChunks(recordSetAccumulated)

    yearMonthRowChunk +: (warningChunks ++ daysRowsChunks ++ monthSummaryRowsChunks)
  }

  private case class DataRecord(
    name: String, missingData: Boolean,
    sharePrice: Option[Double],
    yieldResult: Option[Double], yieldRate: Option[Double],
    accumulatedDays: Option[Int], recordSetAccumulatedDays: Option[Int], months: Int,
    initialBalance: Option[Double],
    balanceChange: Option[Double], shareAmountChange: Option[BigDecimal],
    finalBalance: Option[Double], shareAmount: Option[BigDecimal],
    note: Option[String],
  )

  private object Anchors {
    val Leftmost = 0
    val PostNameSpacing = 1
    val SharePrice = 2
    val PostSharePriceSeparator = 3
    val YieldResult = 4
    val YieldRate = 5
    val AccumulatedDays = 6
    val MonthYieldRate = 7
    val PostYieldSeparator = 8
    val InitialBalance = 9
    val PostInitialBalanceSeparator = 10
    val BalanceChange = 11
    val ShareAmountChange = 12
    val PostChangeSeparator = 13
    val FinalBalance = 14
    val End = 15
  }

  private def toInitialRecordSetChunks(initialRecordSet: InitialRecordSet): Seq[Seq[Chunk]] = {
    val Title = "  Início"

    if (initialRecordSet.records.isEmpty) {
      Seq(
        Seq(Chunk.leftAligned(Anchors.Leftmost, Title)),
        Seq(Chunk.leftAligned(Anchors.Leftmost, "    Nenhum dado")),
      )
    } else {
      toDataRowsChunks(
        Title,
        initialRecordSet.records
          .toSeq.sortBy { case (fund, _) => fund }
          .map { case (fund, initialRecord) =>
            DataRecord(
              s"    $fund", missingData = false,
              initialRecord.sharePrice,
              None, None,
              None, None, 0,
              None,
              None, None,
              initialRecord.finalBalance, initialRecord.shareAmount,
              initialRecord.note,
            )
          },
        DataRecord(
          "    Total", missingData = false,
          None,
          None, None,
          None, None, 0,
          None,
          None, None,
          initialRecordSet.totalFinalBalance, None,
          None,
        )
      )
    }
  }

  private def toRecordSetChunks(recordSet: RecordSet): Seq[Seq[Chunk]] = {
    toDataRowsChunks(
      s"  ${recordSet.date.getDayOfMonth} (${s"+${Words.WithCount.day(recordSet.days)}"})",
      recordSet.records
        .filter { case (_, record) => record.shareAmountChange.isDefined || record.shareAmount.isDefined }
        .toSeq.sortBy { case (fund, _) => fund }.map { case (fund, record) =>
          DataRecord(
            s"    $fund", record.missingData,
            record.sharePrice,
            record.yieldResult, record.yieldRate,
            None, None, 1,
            record.initialBalance,
            record.balanceChange, record.shareAmountChange,
            record.finalBalance, record.shareAmount,
            record.note,
          )
        },
      DataRecord(
        s"    Total", recordSet.missingData,
        None,
        recordSet.totalYieldResult, recordSet.totalYieldRate,
        None, None, 1,
        recordSet.totalInitialBalance,
        recordSet.totalBalanceChange, None,
        recordSet.totalFinalBalance, None,
        None,
      )
    )
  }

  private def toMonthSummaryChunks(recordSetAccumulated: RecordSet.Accumulated): Seq[Seq[Chunk]] =
    makeSummaryChunks(s"  Mês (${Words.WithCount.day(recordSetAccumulated.days)})", recordSetAccumulated, months = 1)

  def makeSummaryChunks(title: String, recordSetAccumulated: RecordSet.Accumulated, months: Int, nameIndentationSize: Int = 4): Seq[Seq[Chunk]] =
    seqIf(options.summary) {
      toDataRowsChunks(
        title,
        recordSetAccumulated.records
          .toSeq.sortBy { case (fund, _) => fund }
          .map { case (fund, recordAccumulated) =>
            DataRecord(
              " " * nameIndentationSize ++ fund, recordAccumulated.missingData,
              None,
              recordAccumulated.yieldResult, recordAccumulated.yieldRate,
              Some(recordAccumulated.days), Some(recordSetAccumulated.days), months,
              None,
              recordAccumulated.balanceChange, recordAccumulated.shareAmountChange,
              None, None,
              None,
            )
          },
        DataRecord(
          " " * nameIndentationSize ++ "Total", recordSetAccumulated.missingData,
          None,
          recordSetAccumulated.totalYieldResult, recordSetAccumulated.totalYieldRate,
          Some(recordSetAccumulated.days), Some(recordSetAccumulated.days), months,
          None,
          recordSetAccumulated.totalBalanceChange, None,
          None, None,
          None,
        )
      )
    }

  private def toDataRowsChunks(title: String, fundsDataRecords: => Seq[DataRecord], totalDataRecord: => DataRecord): Seq[Seq[Chunk]] = {
    val titleRowChunks = Seq(Seq(Chunk.leftAligned(Anchors.Leftmost, title)))
    val fundsRowsChunks = seqIf(options.funds) {
      fundsDataRecords.map(toDataChunks)
    }
    val totalRowChunks = seqIf(options.totals) {
      Seq(toBoldChunks(toDataChunks(totalDataRecord)))
    }

    titleRowChunks ++ fundsRowsChunks ++ totalRowChunks
  }

  private def toDataChunks(dataRecord: DataRecord): Seq[Chunk] = {
    def colorize(value: Double)(format: Double => String): AnsiString = {
      val colorCode = if (value > 0) Code.Blue else Code.Red
      colorCode ++ format(value) ++ Code.DefaultColor
    }

    val monthYieldRate =
      for {
        yieldRate <- dataRecord.yieldRate
        accumulatedDays <- dataRecord.accumulatedDays
        recordSetAccumulatedDays <- dataRecord.recordSetAccumulatedDays
        if (dataRecord.accumulatedDays != dataRecord.recordSetAccumulatedDays && accumulatedDays != 0) || dataRecord.months > 1
        accumulatedDaysPerMonth = recordSetAccumulatedDays / dataRecord.months
      } yield math.pow(1 + yieldRate, accumulatedDaysPerMonth / accumulatedDays.toDouble) - 1

    Seq(
      Seq(
        Chunk.leftAligned(Anchors.Leftmost, dataRecord.name),
        Chunk.rightAligned(Anchors.PostNameSpacing, "  "),
      ),
      Option.when(dataRecord.missingData) {
        val warning = toWarningAnsiString("DADOS FALTANDO")
        Chunk.leftAligned(Anchors.PostNameSpacing, warning.toString, warning.length)
      },
      dataRecord.sharePrice.map(sharePrice => Chunk.rightAligned(Anchors.SharePrice, formatSharePrice(sharePrice))),
      Seq(Chunk.leftAligned(Anchors.PostSharePriceSeparator, " | ")),
      (dataRecord.yieldResult, dataRecord.yieldRate)
        .mapN { (yieldResult, yieldRate) =>
          val yieldRateText = " (" ++ colorize(yieldRate)(formatRate)
          Seq(
            Seq(
              Chunk.rightAligned(Anchors.YieldResult, formatMoneyChange(yieldResult)),
              Chunk.rightAligned(Anchors.YieldRate, yieldRateText.toString, yieldRateText.length),
            ),
            (monthYieldRate, dataRecord.accumulatedDays).tupled
              .fold(
                Seq(Chunk.leftAligned(Anchors.YieldRate, ")"))
              ) { case (monthYieldRate, accumulatedDays) =>
                Seq(
                  Chunk.rightAligned(Anchors.AccumulatedDays, s" em ${Words.WithCount.day(accumulatedDays)};"),
                  Chunk.rightAligned(Anchors.MonthYieldRate, s" ${formatRate(monthYieldRate)} ao mês)"),
                )
              },
          ).flatten
        }
        .toSeq.flatten,
      Seq(Chunk.leftAligned(Anchors.PostYieldSeparator, " | ")),
      dataRecord.initialBalance.map(initialBalance => Chunk.rightAligned(Anchors.InitialBalance, formatMoney(initialBalance))),
      Seq(Chunk.leftAligned(Anchors.PostInitialBalanceSeparator, " | ")),
      dataRecord.balanceChange.map { balanceChange =>
        val balanceChangeText = colorize(balanceChange)(formatMoneyChange)
        Chunk.rightAligned(Anchors.BalanceChange, balanceChangeText.toString, balanceChangeText.length)
      },
      dataRecord.shareAmountChange.map(shareAmountChange => Chunk.rightAligned(Anchors.ShareAmountChange, s" (${formatShareAmountChange(shareAmountChange)})")),
      Seq(Chunk.leftAligned(Anchors.PostChangeSeparator, " | ")),
      dataRecord.finalBalance.map(finalBalance => Chunk.rightAligned(Anchors.FinalBalance, formatMoney(finalBalance))),
      dataRecord.shareAmount.map(shareAmount => Chunk.rightAligned(Anchors.End, s" (${formatShareAmount(shareAmount)})")),
      dataRecord.note.map(note => Chunk.leftAligned(Anchors.End, s"  $note")),
    ).flatten
  }

  private object Words {
    def day(count: Int): String = if (count == 1) "dia" else "dias"

    object WithCount {
      def day(count: Int): String = s"$count ${Words.day(count)}"
    }
  }

  private val TwoDigitsNumberFormat = BrNumber
    .modifyNumberFormat { nf =>
      nf.setMinimumFractionDigits(2)
      nf.setMaximumFractionDigits(2)
    }

  private val TwoDigitsSignedNumberFormat = TwoDigitsNumberFormat.positiveSign(true)

  private val FourDigitsSignedNumberFormat = BrNumber
    .modifyNumberFormat { nf =>
      nf.setMinimumFractionDigits(4)
      nf.setMaximumFractionDigits(4)
    }
    .positiveSign(true)

  private val EightDigitsNumberFormat = BrNumber.modifyNumberFormat { nf =>
    nf.setMinimumFractionDigits(8)
    nf.setMaximumFractionDigits(8)
  }

  private val EightDigitsSignedNumberFormat = EightDigitsNumberFormat.positiveSign(true)

  private def formatMoney(money: Double) = TwoDigitsNumberFormat.formatMoney(money)

  private def formatMoneyChange(moneyChange: Double) = TwoDigitsSignedNumberFormat.formatMoney(moneyChange)

  private def formatSharePrice(sharePrice: Double) = EightDigitsNumberFormat.formatMoney(sharePrice)

  private def formatShareAmount(shareAmount: BigDecimal) = EightDigitsNumberFormat.format(shareAmount.toDouble)

  private def formatShareAmountChange(shareAmountChange: BigDecimal) = EightDigitsSignedNumberFormat.format(shareAmountChange.toDouble)

  private def formatRate(rate: Double) = FourDigitsSignedNumberFormat.formatPercent(rate)

  private def toBoldChunks(chunks: Seq[Chunk]): Seq[Chunk] =
    chunks
      .modify(_.at(0).text).using(text => (Code.Bold ++ text).toString)
      .modify(_.at(chunks.length - 1).text).using(text => (text ++ Code.NormalIntensity).toString)

  private def toWarningAnsiString(text: String): AnsiString = {
    val FormatOn = AnsiString.escape(Code.Bright(Code.White), Code.BG(Code.Red), Code.Bold)
    val FormatOff = AnsiString.escape(Code.DefaultColor, Code.DefaultBgColor, Code.NormalIntensity)
    FormatOn ++ s" $text " ++ FormatOff
  }

  private def seqIf[A](condition: Boolean)(`then`: => Seq[A]): Seq[A] =
    if (condition) `then` else Seq.empty
}

object ChunkMaker {
  case class Options(
    funds: Boolean,
    totals: Boolean,
    days: Boolean,
    summary: Boolean,
  )
}
