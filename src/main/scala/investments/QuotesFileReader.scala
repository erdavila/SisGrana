package sisgrana
package investments

import java.io.InputStream
import java.time.LocalDate
import scala.io.Source

object QuotesFileReader {
  class QuotesRecord(val line: String) {
    lazy val asset: String = field(13, 24).trim

    lazy val date: LocalDate = {
      val str = field(3, 10)
      val yearString = str.substring(0, 4)
      val monthString = str.substring(4, 6)
      val dayString = str.substring(6, 8)
      val dateString = s"$yearString-$monthString-$dayString"
      LocalDate.parse(dateString)
    }

    lazy val openPrice: Double = price(57)
    lazy val maxPrice: Double = price(70)
    lazy val minPrice: Double = price(83)
    lazy val avgPrice: Double = price(96)
    lazy val closePrice: Double = price(109)

    private def price(from: Int): Double =
      field(from, from + 12).toDouble / 100.0

    private def field(from: Int, to: Int): String =
      line.substring(from - 1, to)
  }

  def readFrom(inputStream: InputStream): Iterator[QuotesRecord] =
    for {
      line <- Source.fromInputStream(inputStream).getLines()
      if line.startsWith("01")
    } yield new QuotesRecord(line)
}
