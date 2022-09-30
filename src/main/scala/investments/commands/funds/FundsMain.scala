package sisgrana
package investments.commands.funds

import investments.fileTypes.fundsMonthStatement._
import utils.TextAligner.Chunk
import utils.{BrNumber, TextAligner}
import cats.instances.option._
import cats.syntax.apply._
import java.time.YearMonth

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

    val chunks = yearMonthRowChunk +: initialRecordSetRowsChunks ++: recordSetsRowsChunks

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
    val PostYieldSeparator = 6
    val InitialBalance = 7
    val PostInitialBalanceSeparator = 8
    val BalanceChange = 9
    val ShareAmountChange = 10
    val PostChangeSeparator = 11
    val FinalBalance = 12
    val End = 13
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
        None,
        None, None,
        initialRecordSet.totalFinalBalance, None,
        None,
      )

      titleRowChunks +: recordRowsChunks :+ totalRowChunks
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
        recordSet.totalInitialBalance,
        recordSet.totalBalanceChange, None,
        recordSet.totalFinalBalance, None,
        None,
      )

    dayRowChunks +: recordRowsChunks :+ totalRowChunks
  }

  private def toDataChunks(
    name: String,
    sharePrice: Option[Double],
    yieldResult: Option[Double], yieldRate: Option[Double],
    initialBalance: Option[Double],
    balanceChange: Option[Double], shareAmountChange: Option[BigDecimal],
    finalBalance: Option[Double], shareAmount: Option[BigDecimal],
    note: Option[String],
  ): Seq[Chunk] =
    Seq(
      Seq(
        Chunk.leftAligned(Anchors.Leftmost, s"    $name"),
        Chunk.leftAligned(Anchors.PostNameSpacing, "  "),
      ),
      sharePrice.map(sharePrice => Chunk.rightAligned(Anchors.SharePrice, formatSharePrice(sharePrice))),
      Seq(Chunk.leftAligned(Anchors.PostSharePriceSeparator, " | ")),
      (yieldResult, yieldRate)
        .mapN { (yieldResult, yieldRate) =>
          Seq(
            Chunk.rightAligned(Anchors.YieldResult, formatMoneyChange(yieldResult)),
            Chunk.rightAligned(Anchors.YieldRate, s" (${formatRate(yieldRate)})"),
          )
        }
        .toSeq.flatten,
      Seq(Chunk.leftAligned(Anchors.PostYieldSeparator, " | ")),
      initialBalance.map(initialBalance => Chunk.rightAligned(Anchors.InitialBalance, formatMoney(initialBalance))),
      Seq(Chunk.leftAligned(Anchors.PostInitialBalanceSeparator, " | ")),
      balanceChange.map(balanceChange => Chunk.rightAligned(Anchors.BalanceChange, formatMoneyChange(balanceChange))),
      shareAmountChange.map(shareAmountChange => Chunk.rightAligned(Anchors.ShareAmountChange, s" (${formatShareAmountChange(shareAmountChange)})")),
      Seq(Chunk.leftAligned(Anchors.PostChangeSeparator, " | ")),
      finalBalance.map(finalBalance => Chunk.rightAligned(Anchors.FinalBalance, formatMoney(finalBalance))),
      shareAmount.map(shareAmount => Chunk.rightAligned(Anchors.End, s" (${formatShareAmount(shareAmount)})")),
      note.map(note => Chunk.leftAligned(Anchors.End, s"  $note")),
    ).flatten

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
}
