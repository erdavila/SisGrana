package sisgrana
package investments.commands.funds

import com.softwaremill.quicklens._
import utils.AnsiString.{Format, StringOps}
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
    val colorFormat = if (value > numeric.zero) Format.Blue else Format.Red
    colorFormat(format(value))
  }

  protected def toBoldChunks(chunks: Seq[Chunk]): Seq[Chunk] = {
    val prefix = Format.Bold.rangePrefix
    val suffix = Format.Bold.rangeSuffix

    def modifyChunk(addedPart: AnsiString, f: (String, AnsiString) => AnsiString)(chunk: Chunk) =
      chunk
        .modify(_.text).using(f(_, addedPart).toString)
        .modify(_.width).using(_ + addedPart.length)

    chunks
      .modify(_.at(0))(
        modifyChunk(prefix, (text, part) => part ++ text)
      )
      .modify(_.at(chunks.length - 1))(
        modifyChunk(suffix, (text, part) => text ++ part)
      )
  }

  protected def toWarningAnsiString(text: String): AnsiString =
    (Format.White.bright <|> Format.Red.bg <|> Format.Bold)(s" $text ")

  protected def seqIf[A](condition: Boolean)(`then`: => Seq[A]): Seq[A] =
    if (condition) `then` else Seq.empty
}
