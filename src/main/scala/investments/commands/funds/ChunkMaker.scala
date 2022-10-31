package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import utils.AnsiString.{Code, StringOps}
import utils.TextAligner.Chunk
import utils.{AnsiString, BrNumber}

trait ChunkMaker {
  private val Indentation = "  "

  protected def indented(rowsChunks: Seq[Seq[Chunk]]): Seq[Seq[Chunk]] =
    rowsChunks.map(indentedRow)

  protected def indentedRow(rowChunks: Seq[Chunk]): Seq[Chunk] =
    rowChunks match {
      case h +: t => indentedChunk(h) +: t
      case _ => rowChunks
    }

  protected def indentedChunk(chunk: Chunk): Chunk =
    chunk
      .modify(_.text).using(Indentation ++ _)
      .modify(_.width).using(_ + Indentation.length)

  protected object Words {
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

  protected val EightDigitsNumberFormat: BrNumber = BrNumber.modifyNumberFormat { nf =>
    nf.setMinimumFractionDigits(8)
    nf.setMaximumFractionDigits(8)
  }

  private val EightDigitsSignedNumberFormat = EightDigitsNumberFormat.positiveSign(true)

  protected def formatMoney(money: Double): String = TwoDigitsNumberFormat.formatMoney(money)

  protected def formatMoneyChange(moneyChange: Double): String = TwoDigitsSignedNumberFormat.formatMoney(moneyChange)

  protected def formatSharePrice(sharePrice: Double): String = EightDigitsNumberFormat.formatMoney(sharePrice)

  protected def formatShareAmount(shareAmount: BigDecimal): String = EightDigitsNumberFormat.format(shareAmount.toDouble)

  protected def formatShareAmountChange(shareAmountChange: BigDecimal): String = EightDigitsSignedNumberFormat.format(shareAmountChange.toDouble)

  protected def formatRate(rate: Double): String = FourDigitsSignedNumberFormat.formatPercent(rate)

  protected def colorize[A](value: A)(format: A => String)(implicit numeric: Numeric[A]): AnsiString = {
    import numeric._
    val colorCode = if (value > numeric.zero) Code.Blue else Code.Red
    colorCode ++ format(value) ++ Code.DefaultColor
  }

  protected def toBoldChunks(chunks: Seq[Chunk]): Seq[Chunk] =
    chunks
      .modify(_.at(0).text).using(text => (Code.Bold ++ text).toString)
      .modify(_.at(chunks.length - 1).text).using(text => (text ++ Code.NormalIntensity).toString)

  protected def toWarningAnsiString(text: String): AnsiString = {
    val FormatOn = AnsiString.escape(Code.Bright(Code.White), Code.BG(Code.Red), Code.Bold)
    val FormatOff = AnsiString.escape(Code.DefaultColor, Code.DefaultBgColor, Code.NormalIntensity)
    FormatOn ++ s" $text " ++ FormatOff
  }

  protected def seqIf[A](condition: Boolean)(`then`: => Seq[A]): Seq[A] =
    if (condition) `then` else Seq.empty
}
