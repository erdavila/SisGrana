package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.Rate
import java.time.YearMonth
import java.time.temporal.ChronoUnit
import utils.AnsiString.{Code, StringOps}
import utils.TextAligner.Chunk
import utils.{AnsiString, BrNumber}

class ChunkMaker(options: ChunkMaker.Options) {
  def makeChunks(
    yearMonth: YearMonth,
    showInitialDataDifferWarning: Boolean,
    initialPositionRecordSet: Option[RecordSet.Position.Initial],
    recordSets: Seq[RecordSet],
  ): Seq[Seq[Chunk]] = {
    val yearMonthRowChunk = Seq(Chunk.leftAligned(Anchors.Leftmost, yearMonth.toString))

    val warningChunks = seqIf(showInitialDataDifferWarning) {
      val warning = toWarningAnsiString("DADOS INICIAIS DIFEREM DOS DADOS FINAIS DO MÊS ANTERIOR")
      Seq(Seq(indentedChunk(Chunk.leftAligned(Anchors.Leftmost, warning.toString, warning.length))))
    }

    val daysRowsChunks = seqIf(options.days) {
      val initialRecordSetRowsChunks = initialPositionRecordSet
        .toSeq
        .flatMap(toInitialRecordSetChunks)

      val recordSetsRowsChunks = recordSets.flatMap(toRecordSetChunks)
      initialRecordSetRowsChunks ++ recordSetsRowsChunks
    }

    val monthSummaryRowsChunks = makeMonthSummaryChunks(recordSets.last.accumulated)

    yearMonthRowChunk +: indented(warningChunks ++ daysRowsChunks ++ monthSummaryRowsChunks)
  }

  private case class DataRecord(
    name: String, missingData: Boolean,
    sharePrice: Option[Double],
    yieldResult: Option[Double], yieldRate: Option[Rate],
    monthYieldRate: Option[Rate], showYieldRateDays: Boolean,
    initialBalance: Option[Double],
    balanceChange: Option[Double], shareAmountChange: Option[BigDecimal],
    finalBalance: Option[Double], shareAmount: Option[BigDecimal],
    note: Option[String],
  )

  private val Spacing = "  "

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

  private def toInitialRecordSetChunks(initialPositionRecordSet: RecordSet.Position.Initial): Seq[Seq[Chunk]] = {
    val Title = "Início"

    if (initialPositionRecordSet.positionRecords.isEmpty) {
      Seq(
        Seq(Chunk.leftAligned(Anchors.Leftmost, Title)),
        Seq(indentedChunk(Chunk.leftAligned(Anchors.Leftmost, "Nenhum dado"))),
      )
    } else {
      toDataRowsChunks(
        Title,
        initialPositionRecordSet.positionRecords
          .view.mapValues(_.value)
          .toSeq.sortBy { case (fund, _) => fund }
          .map { case (fund, initialPositionRecord) =>
            DataRecord(
              fund, missingData = false,
              Some(initialPositionRecord.sharePrice),
              None, None,
              None, showYieldRateDays = false,
              None,
              None, None,
              initialPositionRecord.finalBalance, initialPositionRecord.shareAmount,
              initialPositionRecord.note,
            )
          },
        DataRecord(
          "Total", missingData = false,
          None,
          None, None,
          None, showYieldRateDays = false,
          None,
          None, None,
          initialPositionRecordSet.totalFinalBalance, None,
          None,
        )
      )
    }
  }

  private def toRecordSetChunks(recordSet: RecordSet): Seq[Seq[Chunk]] = {
    toDataRowsChunks(
      s"${recordSet.position.date.getDayOfMonth} (${s"+${Words.WithCount.day(recordSet.position.days)}"})",
      recordSet.records
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, record) =>
          val presentPositionRecord = record.position.flatten

          DataRecord(
            fund, record.accumulated.missingData,
            presentPositionRecord.map(_.sharePrice),
            presentPositionRecord.flatMap(_.yieldResult), presentPositionRecord.flatMap(_.yieldRate),
            None, showYieldRateDays = false,
            presentPositionRecord.flatMap(_.initialBalance),
            presentPositionRecord.flatMap(_.balanceChange), presentPositionRecord.flatMap(_.shareAmountChange),
            presentPositionRecord.flatMap(_.finalBalance), presentPositionRecord.flatMap(_.shareAmount),
            presentPositionRecord.flatMap(_.note),
          )
        },
      DataRecord(
        "Total", recordSet.accumulated.missingData,
        None,
        recordSet.position.totalYieldResult, recordSet.position.totalYieldRate,
        None, showYieldRateDays = false,
        recordSet.position.totalInitialBalance,
        recordSet.position.totalBalanceChange, None,
        recordSet.position.totalFinalBalance, None,
        None,
      )
    )
  }

  private def makeMonthSummaryChunks(accumulatedRecordSet: RecordSet.Accumulated): Seq[Seq[Chunk]] =
    makeSummaryChunks(s"Mês (${Words.WithCount.day(accumulatedRecordSet.days)})", accumulatedRecordSet, months = 1)

  def makeMonthRangeSummaryChunks(initialMonth: YearMonth, finalMonth: YearMonth, accumulatedRecordSet: RecordSet.Accumulated): Seq[Seq[Chunk]] = {
    val months = 1 + initialMonth.until(finalMonth, ChronoUnit.MONTHS).toInt
    seqIf(months > 1) {
      makeSummaryChunks(
        s"Meses de ${initialMonth} a ${finalMonth} (${months} meses/${accumulatedRecordSet.days} dias)",
        accumulatedRecordSet,
        months = months,
      )
    }
  }

  private def makeSummaryChunks(
    title: String,
    accumulatedRecordSet: RecordSet.Accumulated,
    months: Int,
  ): Seq[Seq[Chunk]] = {
    def toMonthlyRate(rate: Option[Rate]) =
      for {
        rate <- rate
        daysPerMonth = accumulatedRecordSet.days / months
        if rate.days != daysPerMonth
      } yield rate.convert(daysPerMonth)

    def shouldShowYieldRateDays(rate: Option[Rate]) =
      rate match {
        case Some(rate) => rate.days != accumulatedRecordSet.days
        case None => false
      }

    seqIf(options.summary) {
      toDataRowsChunks(
        title,
        accumulatedRecordSet.records
          .toSeq.sortBy { case (fund, _) => fund }
          .map { case (fund, accumulatedRecord) =>
            DataRecord(
              fund, accumulatedRecord.missingData,
              None,
              accumulatedRecord.yieldResult, accumulatedRecord.yieldRate,
              toMonthlyRate(accumulatedRecord.yieldRate), shouldShowYieldRateDays(accumulatedRecord.yieldRate),
              None,
              accumulatedRecord.balanceChange, accumulatedRecord.shareAmountChange,
              None, None,
              None,
            )
          },
        DataRecord(
          "Total", accumulatedRecordSet.missingData,
          None,
          accumulatedRecordSet.totalYieldResult, accumulatedRecordSet.totalYieldRate,
          toMonthlyRate(accumulatedRecordSet.totalYieldRate), shouldShowYieldRateDays(accumulatedRecordSet.totalYieldRate),
          None,
          accumulatedRecordSet.totalBalanceChange, None,
          None, None,
          None,
        )
      )
    }
  }

  private def toDataRowsChunks(title: String, fundsDataRecords: => Seq[DataRecord], totalDataRecord: => DataRecord): Seq[Seq[Chunk]] = {
    val titleRowChunks = Seq(Seq(Chunk.leftAligned(Anchors.Leftmost, title)))
    val fundsRowsChunks = seqIf(options.funds) {
      fundsDataRecords.map(toDataChunks)
    }
    val totalRowChunks = seqIf(options.totals) {
      Seq(toBoldChunks(toDataChunks(totalDataRecord)))
    }

    titleRowChunks ++ indented(fundsRowsChunks ++ totalRowChunks)
  }

  private def toDataChunks(dataRecord: DataRecord): Seq[Chunk] = {
    def colorize(value: Double)(format: Double => String): AnsiString = {
      val colorCode = if (value > 0) Code.Blue else Code.Red
      colorCode ++ format(value) ++ Code.DefaultColor
    }

    Seq(
      Seq(
        Chunk.leftAligned(Anchors.Leftmost, dataRecord.name),
        Chunk.rightAligned(Anchors.PostNameSpacing, Spacing),
      ),
      Option.when(dataRecord.missingData) {
        val warning = toWarningAnsiString("DADOS FALTANDO")
        Chunk.leftAligned(Anchors.PostNameSpacing, warning.toString, warning.length)
      },
      dataRecord.sharePrice.map(sharePrice => Chunk.rightAligned(Anchors.SharePrice, formatSharePrice(sharePrice))),
      Seq(Chunk.leftAligned(Anchors.PostSharePriceSeparator, " | ")),
      (dataRecord.yieldResult, dataRecord.yieldRate)
        .mapN { (yieldResult, yieldRate) =>
          val yieldRateText = " (" ++ colorize(yieldRate.value)(formatRate)
          Seq(
            Seq(
              Chunk.rightAligned(Anchors.YieldResult, formatMoneyChange(yieldResult)),
              Chunk.rightAligned(Anchors.YieldRate, yieldRateText.toString, yieldRateText.length),
            ),
            dataRecord.monthYieldRate match {
              case Some(monthYieldRate) =>
                Seq(
                  if (dataRecord.showYieldRateDays) {
                    Chunk.rightAligned(Anchors.AccumulatedDays, s" em ${Words.WithCount.day(yieldRate.days)};")
                  } else {
                    Chunk.leftAligned(Anchors.YieldRate, ";")
                  },
                  Chunk.rightAligned(Anchors.MonthYieldRate, s" ${formatRate(monthYieldRate.value)} ao mês)"),
                )
              case None => Seq(Chunk.leftAligned(Anchors.YieldRate, ")"))
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
      dataRecord.note.map(note => Chunk.leftAligned(Anchors.End, s"$Spacing$note")),
    ).flatten
  }

  private val Indentation = "  "

  private def indented(rowsChunks: Seq[Seq[Chunk]]): Seq[Seq[Chunk]] =
    rowsChunks.map(indentedRow)

  private def indentedRow(rowChunks: Seq[Chunk]): Seq[Chunk] =
    rowChunks match {
      case h +: t => indentedChunk(h) +: t
      case _ => rowChunks
    }

  private def indentedChunk(chunk: Chunk): Chunk =
    chunk
      .modify(_.text).using(Indentation ++ _)
      .modify(_.width).using(_ + Indentation.length)

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
