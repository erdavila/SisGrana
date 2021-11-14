package sisgrana
package investments.multiImport.nonQuoteDates

import investments.model.NonQuoteDate
import java.io.InputStream
import java.time.LocalDate
import utils.SSV

object NonQuoteDatesFileReader {
  def readFrom(inputStream: InputStream): Iterator[NonQuoteDate] = {
    for {
      Seq(string) <- SSV.readFrom(inputStream)
      date = LocalDate.parse(string)
    } yield NonQuoteDate(date)
  }
}
