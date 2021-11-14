package sisgrana
package investments.fileTypes.nonQuoteDates

import investments.files.SSV
import java.io.InputStream
import java.time.LocalDate

object NonQuoteDatesFileReader {
  def readFrom(inputStream: InputStream): Iterator[NonQuoteDate] = {
    for {
      Seq(string) <- SSV.readFrom(inputStream)
      date = LocalDate.parse(string)
    } yield NonQuoteDate(date)
  }
}
