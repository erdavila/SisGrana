package sisgrana
package investments.utils

import java.text.{NumberFormat, ParseException, ParsePosition}
import java.util.Locale

object BrNumber {
  private val NF = NumberFormat.getInstance(Locale.forLanguageTag("pt-br"))

  def parse(string: String): Double = {
    val parsePosition = new ParsePosition(0)
    val number = NF.parse(string, parsePosition).doubleValue()
    if (parsePosition.getIndex != string.length) {
      throw new ParseException(s"Invalid number: $string", parsePosition.getIndex)
    }
    number
  }

  def format(number: Double): String =
    NF.format(number)

  def formatMoney(number: Double): String =
    s"R$$ ${format(number)}"

  def formatPercent(number: Double): String =
    s"${format(100 * number)}%"
}
