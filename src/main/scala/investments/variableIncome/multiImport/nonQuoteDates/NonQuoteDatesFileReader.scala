package sisgrana
package investments.variableIncome.multiImport.nonQuoteDates

import investments.variableIncome.model.NonQuoteDate
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
