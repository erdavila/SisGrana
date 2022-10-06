package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import java.time.YearMonth
import utils.AnsiString.{Code, StringOps}
import utils.TextAligner.Chunk
import utils.{AnsiString, BrNumber, TextAligner}

class Printer(
  accumulated: Boolean,
  funds: Boolean,
  totals: Boolean,
) {
  def printMonthRecordSets(yearMonth: YearMonth, initialRecordSet: InitialRecordSet, recordSets: Seq[RecordSet]): Unit = {
    val yearMonthRowChunk = Seq(Chunk.leftAligned(Anchors.Leftmost, yearMonth.toString))
    val initialRecordSetRowsChunks = toInitialRecordSetChunks(initialRecordSet)
    val recordSetsRowsChunks = recordSets.flatMap(toRecordSetChunks)
    val monthSummaryRowsChunks = seqIf(!accumulated) {
      recordSets.lastOption.toSeq.flatMap(toMonthSummaryChunks)
    }

    val chunks = yearMonthRowChunk +: initialRecordSetRowsChunks ++: recordSetsRowsChunks ++: monthSummaryRowsChunks

    TextAligner.alignAndRender(chunks)
      .foreach(println)
  }

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
    val titleRowChunks = Seq(Chunk.leftAligned(Anchors.Leftmost, "  Início"))

    if (initialRecordSet.records.isEmpty) {
      val noDataRowChunks = Seq(Chunk.leftAligned(Anchors.Leftmost, "    Nenhum dado"))

      Seq(
        titleRowChunks,
        noDataRowChunks,
      )
    } else {
      val recordRowsChunks = seqIf(funds) {
        initialRecordSet.records
          .toSeq.sortBy { case (fund, _) => fund }
          .map { case (fund, initialRecord) =>
            toDataChunks(
              fund, missingData = false,
              initialRecord.sharePrice,
              None, None,
              initialRecord.accumulatedDays, initialRecordSet.accumulatedDays, None,
              None,
              None, None,
              initialRecord.finalBalance, initialRecord.shareAmount,
              initialRecord.note,
            )
          }
      }
      val totalRowChunks = seqIf(totals) {
        Seq(
          toBoldChunks(
            toDataChunks(
              "Total", missingData = false,
              None,
              None, None,
              initialRecordSet.accumulatedDays, initialRecordSet.accumulatedDays, None,
              None,
              None, None,
              initialRecordSet.totalFinalBalance, None,
              None,
            )
          )
        )
      }

      titleRowChunks +: (recordRowsChunks ++ totalRowChunks)
    }
  }

  private def toRecordSetChunks(recordSet: RecordSet): Seq[Seq[Chunk]] = {
    val dayDetail = if (accumulated) {
      Words.WithCount.day(recordSet.accumulatedDays)
    } else {
      s"+${Words.WithCount.day(recordSet.days)}"
    }

    val dayRowChunks = Seq(Chunk.leftAligned(Anchors.Leftmost, s"  ${recordSet.date.getDayOfMonth} ($dayDetail)"))
    val recordRowsChunks = seqIf(funds) {
      recordSet.records
        .filter { case (_, record) => accumulated || record.shareAmountChange.isDefined || record.shareAmount.isDefined }
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, record) =>
          if (accumulated) {
            toDataChunks(
              fund, record.missingData,
              record.sharePrice,
              record.accumulatedYieldResult, record.accumulatedYieldRate,
              record.accumulatedDays, recordSet.accumulatedDays, record.accumulatedYieldRate,
              None,
              record.accumulatedBalanceChange, record.accumulatedShareAmountChange,
              record.finalBalance, record.shareAmount,
              record.note,
            )
          } else {
            toDataChunks(
              fund, record.missingData,
              record.sharePrice,
              record.yieldResult, record.yieldRate,
              record.accumulatedDays, recordSet.accumulatedDays, None,
              record.initialBalance,
              record.balanceChange, record.shareAmountChange,
              record.finalBalance, record.shareAmount,
              record.note,
            )
          }
        }
    }

    val totalRowChunks = seqIf(totals) {
      val chunks = if (accumulated) {
        toDataChunks(
          "Total", recordSet.missingData,
          None,
          recordSet.accumulatedTotalYieldResult, recordSet.accumulatedTotalYieldRate,
          recordSet.accumulatedDays, recordSet.accumulatedDays, None,
          None,
          recordSet.accumulatedTotalBalanceChange, None,
          recordSet.totalFinalBalance, None,
          None,
        )
      } else {
        toDataChunks(
          "Total", recordSet.missingData,
          None,
          recordSet.totalYieldResult, recordSet.totalYieldRate,
          recordSet.accumulatedDays, recordSet.accumulatedDays, None,
          recordSet.totalInitialBalance,
          recordSet.totalBalanceChange, None,
          recordSet.totalFinalBalance, None,
          None,
        )
      }

      Seq(toBoldChunks(chunks))
    }

    dayRowChunks +: (recordRowsChunks ++ totalRowChunks)
  }

  private def toMonthSummaryChunks(recordSet: RecordSet): Seq[Seq[Chunk]] = {
    val titleRowChunks = Seq(Chunk.leftAligned(Anchors.Leftmost, s"  Mês (${Words.WithCount.day(recordSet.accumulatedDays)})"))
    val recordsRowsChunks = seqIf(funds) {
      recordSet.records
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, record) => toMonthSummaryRecordChunks(fund, record, recordSet.accumulatedDays) }
    }
    val monthSummaryTotalRowsChunks = toMonthSummaryTotalChunks(recordSet)

    titleRowChunks +: (recordsRowsChunks ++ monthSummaryTotalRowsChunks)
  }

  private def toMonthSummaryRecordChunks(fund: String, record: Record, recordSetAccumulatedDays: Int): Seq[Chunk] =
    toDataChunks(
      fund, record.missingData,
      None,
      record.accumulatedYieldResult, record.accumulatedYieldRate,
      record.accumulatedDays, recordSetAccumulatedDays, record.accumulatedYieldRate,
      None,
      record.accumulatedBalanceChange, record.accumulatedShareAmountChange,
      None, None,
      None,
    )

  private def toMonthSummaryTotalChunks(recordSet: RecordSet): Seq[Seq[Chunk]] =
    seqIf(totals) {
      Seq(
        toBoldChunks(
          toDataChunks(
            "Total", recordSet.missingData,
            None,
            recordSet.accumulatedTotalYieldResult, recordSet.accumulatedTotalYieldRate,
            recordSet.accumulatedDays, recordSet.accumulatedDays, None,
            None,
            recordSet.accumulatedTotalBalanceChange, None,
            None, None,
            None,
          )
        )
      )
    }

  private def toDataChunks(
    name: String, missingData: Boolean,
    sharePrice: Option[Double],
    yieldResult: Option[Double], yieldRate: Option[Double],
    accumulatedDays: Int, recordSetAccumulatedDays: Int, accumulatedYieldRate: Option[Double],
    initialBalance: Option[Double],
    balanceChange: Option[Double], shareAmountChange: Option[BigDecimal],
    finalBalance: Option[Double], shareAmount: Option[BigDecimal],
    note: Option[String],
  ): Seq[Chunk] = {
    def colorize(value: Double)(format: Double => String): AnsiString = {
      val colorCode = if (value > 0) Code.Blue else Code.Red
      colorCode ++ format(value) ++ Code.DefaultColor
    }

    val monthYieldRate =
      for {
        accumulatedYieldRate <- accumulatedYieldRate
        if accumulatedDays != recordSetAccumulatedDays && accumulatedDays != 0
      } yield math.pow(1 + accumulatedYieldRate, recordSetAccumulatedDays / accumulatedDays.toDouble) - 1

    Seq(
      Seq(
        Chunk.leftAligned(Anchors.Leftmost, s"    $name"),
        Chunk.rightAligned(Anchors.PostNameSpacing, "  "),
      ),
      Option.when(missingData) {
        val FormatOn = AnsiString.escape(Code.Bright(Code.White), Code.BG(Code.Red), Code.Bold)
        val FormatOff = AnsiString.escape(Code.DefaultColor, Code.DefaultBgColor, Code.NormalIntensity)
        val message = FormatOn ++ " DADOS FALTANDO " ++ FormatOff
        Chunk.leftAligned(Anchors.PostNameSpacing, message.toString, message.length)
      },
      sharePrice.map(sharePrice => Chunk.rightAligned(Anchors.SharePrice, formatSharePrice(sharePrice))),
      Seq(Chunk.leftAligned(Anchors.PostSharePriceSeparator, " | ")),
      (yieldResult, yieldRate)
        .mapN { (yieldResult, yieldRate) =>
          val yieldRateText = " (" ++ colorize(yieldRate)(formatRate)
          Seq(
            Seq(
              Chunk.rightAligned(Anchors.YieldResult, formatMoneyChange(yieldResult)),
              Chunk.rightAligned(Anchors.YieldRate, yieldRateText.toString, yieldRateText.length),
            ),
            monthYieldRate.fold(
              Seq(Chunk.leftAligned(Anchors.YieldRate, ")"))
            )(monthYieldRate =>
              Seq(
                Chunk.rightAligned(Anchors.AccumulatedDays, s" em ${Words.WithCount.day(accumulatedDays)};"),
                Chunk.rightAligned(Anchors.MonthYieldRate, s" ${formatRate(monthYieldRate)} no mês)"),
              )
            ),
          ).flatten
        }
        .toSeq.flatten,
      Seq(Chunk.leftAligned(Anchors.PostYieldSeparator, " | ")),
      seqIf(!accumulated) {
        Seq(
          initialBalance.map(initialBalance => Chunk.rightAligned(Anchors.InitialBalance, formatMoney(initialBalance))),
          Seq(Chunk.leftAligned(Anchors.PostInitialBalanceSeparator, " | ")),
        ).flatten
      },
      balanceChange.map { balanceChange =>
        val balanceChangeText = colorize(balanceChange)(formatMoneyChange)
        Chunk.rightAligned(Anchors.BalanceChange, balanceChangeText.toString, balanceChangeText.length)
      },
      shareAmountChange.map(shareAmountChange => Chunk.rightAligned(Anchors.ShareAmountChange, s" (${formatShareAmountChange(shareAmountChange)})")),
      Seq(Chunk.leftAligned(Anchors.PostChangeSeparator, " | ")),
      finalBalance.map(finalBalance => Chunk.rightAligned(Anchors.FinalBalance, formatMoney(finalBalance))),
      shareAmount.map(shareAmount => Chunk.rightAligned(Anchors.End, s" (${formatShareAmount(shareAmount)})")),
      note.map(note => Chunk.leftAligned(Anchors.End, s"  $note")),
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

  private def seqIf[A](condition: Boolean)(`then`: => Seq[A]): Seq[A] =
    if (condition) `then` else Seq.empty
}
