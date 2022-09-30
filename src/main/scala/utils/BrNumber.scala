package sisgrana
package utils

import java.text.{NumberFormat, ParseException, ParsePosition}
import java.util.Locale

class BrNumber private(nf: NumberFormat, positiveSign: Boolean) {
  def parse(string: String): Double = {
    val parsePosition = new ParsePosition(0)
    val number = nf.parse(string, parsePosition).doubleValue()
    if (parsePosition.getIndex != string.length) {
      throw new ParseException(s"Invalid number: $string", parsePosition.getIndex)
    }
    number
  }

  def parseBigDecimal(string: String): BigDecimal = {
    val cleanedString = string
      .replace(".", "")
      .replace(",", ".")
    BigDecimal(cleanedString)
  }

  def format(number: Double): String = {
    val maybeSign = if (number > 0 && positiveSign) "+" else ""
    maybeSign ++ nf.format(number)
  }

  def formatMoney(number: Double): String =
    s"R$$ ${format(number)}"

  def formatPercent(number: Double): String =
    s"${format(100 * number)}%"

  def modifyNumberFormat(modify: NumberFormat => Unit): BrNumber = {
    val nf = this.nf.clone().asInstanceOf[NumberFormat]
    modify(nf)
    new BrNumber(nf, this.positiveSign)
  }

  def positiveSign(positiveSign: Boolean): BrNumber =
    new BrNumber(this.nf, positiveSign)
}

object BrNumber extends BrNumber(
  NumberFormat.getInstance(Locale.forLanguageTag("pt-br")),
  positiveSign = false,
)
