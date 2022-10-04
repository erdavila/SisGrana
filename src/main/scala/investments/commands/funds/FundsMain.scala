package sisgrana
package investments.commands.funds

import cats.instances.option._
import cats.syntax.apply._
import com.softwaremill.quicklens._
import investments.fileTypes.fundsMonthStatement._
import java.time.YearMonth
import utils.AnsiString.{Code, StringOps}
import utils.TextAligner.Chunk
import utils.{AnsiString, BrNumber, TextAligner}

object FundsMain {
  // TODO: warn when there is no sharePrice for a previousRecord with shareAmount > 0
  // TODO: warn when there is no no data for the last day of the month
  // TODO: when listing multiple months, check if final recordSet from previous month matches initialRecordSet of current month

  def main(args: Array[String]): Unit = {
    val yearMonth = YearMonth.parse(args(0))
    val statement = FundsMonthStatementFileReader.read(yearMonth)
    val (initialRecordSet, recordSets) = StatementProcessor.process(yearMonth, statement)

    printMonthRecordSets(yearMonth, initialRecordSet, recordSets)
  }

  private def printMonthRecordSets(yearMonth: YearMonth, initialRecordSet: InitialRecordSet, recordSets: Seq[RecordSet]): Unit = {
    val yearMonthRowChunk = Seq(Chunk.leftAligned(Anchors.Leftmost, yearMonth.toString));
    val initialRecordSetRowsChunks = toInitialRecordSetChunks(initialRecordSet)
    val recordSetsRowsChunks = recordSets.flatMap(toRecordSetChunks)
    val monthSummaryRowsChunks = recordSets.lastOption.toSeq.flatMap(toMonthSummaryChunks)

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
      val recordRowsChunks = initialRecordSet.records
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, initialRecord) =>
          toDataChunks(
            fund,
            initialRecord.sharePrice,
            None, None,
            initialRecord.accumulatedDays, None,
            None,
            None, None,
            initialRecord.finalBalance, initialRecord.shareAmount,
            initialRecord.note,
          )
        }
      val totalRowChunks = toDataChunks(
        "Total",
        None,
        None, None,
        initialRecordSet.accumulatedDays, None,
        None,
        None, None,
        initialRecordSet.totalFinalBalance, None,
        None,
      )

      titleRowChunks +: recordRowsChunks :+ toBoldChunks(totalRowChunks)
    }
  }

  private def toRecordSetChunks(recordSet: RecordSet): Seq[Seq[Chunk]] = {
    val dayRowChunks = Seq(Chunk.leftAligned(Anchors.Leftmost, s"  ${recordSet.date.getDayOfMonth} (+${recordSet.days} ${Words.day(recordSet.days)})"))
    val recordRowsChunks = recordSet.records
        .filter { case (_, record) => record.initialBalance.isDefined || record.finalBalance.isDefined }
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, record) =>
          toDataChunks(
            fund,
            record.sharePrice,
            record.yieldResult, record.yieldRate,
            record.accumulatedDays, None,
            record.initialBalance,
            record.balanceChange, record.shareAmountChange,
            record.finalBalance, record.shareAmount,
            record.note,
          )
        }
    val totalRowChunks =
      toDataChunks(
        "Total",
        None,
        recordSet.totalYieldResult, recordSet.totalYieldRate,
        recordSet.accumulatedDays, None,
        recordSet.totalInitialBalance,
        recordSet.totalBalanceChange, None,
        recordSet.totalFinalBalance, None,
        None,
      )

    dayRowChunks +: recordRowsChunks :+ toBoldChunks(totalRowChunks)
  }

  private def toMonthSummaryChunks(recordSet: RecordSet): Seq[Seq[Chunk]] =
    Seq(
      Seq(Seq(Chunk.leftAligned(Anchors.Leftmost, s"  Mês (${recordSet.accumulatedDays} ${Words.day(recordSet.accumulatedDays)})"))),
      recordSet.records
        .toSeq.sortBy { case (fund, _) => fund }
        .map { case (fund, record) => toMonthSummaryRecordChunks(fund, record, recordSet.accumulatedDays) },
      Seq(toMonthSummaryTotalChunks(recordSet)),
  ).flatten

  private def toMonthSummaryRecordChunks(fund: String, record: Record, recordSetAccumulatedDays: Int): Seq[Chunk] = {
    val monthYieldRate =
      for {
        accumulatedYieldRate <- record.accumulatedYieldRate
        if record.accumulatedDays != recordSetAccumulatedDays && record.accumulatedDays != 0
      } yield math.pow(1 + accumulatedYieldRate, recordSetAccumulatedDays / record.accumulatedDays.toDouble) - 1
    toDataChunks(
      fund,
      None,
      record.accumulatedYieldResult, record.accumulatedYieldRate,
      record.accumulatedDays, monthYieldRate,
      None,
      record.accumulatedBalanceChange, record.accumulatedShareAmountChange,
      None, None,
      None,
    )
  }

  private def toMonthSummaryTotalChunks(recordSet: RecordSet): Seq[Chunk] = {
    val chunks = toDataChunks(
      "Total",
      None,
      recordSet.accumulatedTotalYieldResult, recordSet.accumulatedTotalYieldRate,
      recordSet.accumulatedDays, None,
      None,
      recordSet.accumulatedTotalBalanceChange, None,
      None, None,
      None,
    )

    toBoldChunks(chunks)
  }

  private def toDataChunks(
    name: String,
    sharePrice: Option[Double],
    yieldResult: Option[Double], yieldRate: Option[Double],
    accumulatedDays: Int, monthYieldRate: Option[Double],
    initialBalance: Option[Double],
    balanceChange: Option[Double], shareAmountChange: Option[BigDecimal],
    finalBalance: Option[Double], shareAmount: Option[BigDecimal],
    note: Option[String],
  ): Seq[Chunk] = {
    def colorize(value: Double)(format: Double => String): AnsiString = {
      val colorCode = if (value > 0) Code.Blue else Code.Red
      colorCode ++ format(value) ++ Code.DefaultColor
    }

    Seq(
      Seq(
        Chunk.leftAligned(Anchors.Leftmost, s"    $name"),
        Chunk.leftAligned(Anchors.PostNameSpacing, "  "),
      ),
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
                Chunk.rightAligned(Anchors.AccumulatedDays, s" em $accumulatedDays ${Words.day(accumulatedDays)};"),
                Chunk.rightAligned(Anchors.MonthYieldRate, s" ${formatRate(monthYieldRate)} no mês)"),
              )
            ),
          ).flatten
        }
        .toSeq.flatten,
      Seq(Chunk.leftAligned(Anchors.PostYieldSeparator, " | ")),
      initialBalance.map(initialBalance => Chunk.rightAligned(Anchors.InitialBalance, formatMoney(initialBalance))),
      Seq(Chunk.leftAligned(Anchors.PostInitialBalanceSeparator, " | ")),
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
}
