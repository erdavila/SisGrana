package sisgrana
package investments.variableIncome.detectEvent

import investments.utils.BrNumber
import investments.variableIncome.QuotesFileReader
import investments.variableIncome.files.filters.Filter
import investments.variableIncome.files.{FilePathResolver, filters}
import java.io.InputStream
import java.time.LocalDate

object DetectEventMain {
  private case class DateQuote(date: LocalDate, quote: Double)

  def main(args: Array[String]): Unit = {
    val (asset, minVariation, paths) = ArgsParser.parse(args)
    val filePaths = FilePathResolver.resolve(paths)
    val inputFiles = Filter.apply(filters.QuotesFiles)(filePaths)

    val dateQuotes = for {
      inputFile <- inputFiles
      dateQuote <- inputFile.path.read { inputStream =>
        readQuotes(asset, inputStream).toSeq
      }
    } yield dateQuote

    val sortedDateQuotes = dateQuotes.groupMapReduce(_.date)(_.quote)((q, _) => q)
      .map(DateQuote.tupled)
      .toSeq
      .sortBy(_.date)

    if (sortedDateQuotes.isEmpty) {
      println(s"Nenhuma cotação encontrada para $asset!")
    } else {
      for {
        (label, date, quote) <- Seq(
          ("inicial", sortedDateQuotes.head.date, sortedDateQuotes.head.quote),
          ("final  ", sortedDateQuotes.last.date, sortedDateQuotes.last.quote),
        )
      } {
        println(s"Cotação $label ($date): ${BrNumber.formatMoney(quote)}")
      }

      for {
        Seq(dq1, dq2) <- sortedDateQuotes.sliding(2)
        rate = (dq2.quote - dq1.quote) / dq1.quote
        if math.abs(rate) >= minVariation
      } {
        print(s"${dq1.date} ${dq2.date}:")
        print(s"  ${BrNumber.formatMoney(dq1.quote)} -> ${BrNumber.formatMoney(dq2.quote)}")
        print(s"  (${BrNumber.formatPercent(rate)})")
        print(s"  1:${BrNumber.format(dq2.quote / dq1.quote)}")
        print(s"  ${BrNumber.format(dq1.quote / dq2.quote)}:1")
        println()
      }
    }
  }

  private def readQuotes(asset: String, inputStream: InputStream) =
    for {
      record <- QuotesFileReader.readFrom(inputStream)
      if record.asset == asset
    } yield DateQuote(record.date, record.closePrice)
}
