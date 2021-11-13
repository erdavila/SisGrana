package sisgrana
package investments.variableIncome.processQuotes

import investments.utils.BrNumber
import investments.variableIncome.QuotesFileReader
import investments.variableIncome.files.InputFile
import investments.variableIncome.files.filters.applyFilter
import investments.variableIncome.quotesFiles.QuotesFileName
import java.io.InputStream
import java.time.LocalDate
import utils.{DateRanges, PredicateBinarySearch}

object ProcessQuotesMain {
  private case class DateQuote(date: LocalDate, quote: Double)

  def main(args: Array[String]): Unit = {
    val (operation, filePaths) = ArgsParser.parse(args)
    val inputFiles = applyFilter(QuotesFileName.FilesFilter)(filePaths)

    operation match {
      case OperationArguments.FindVariation(asset, minVariation) => findVariation(asset, minVariation, inputFiles)
      case OperationArguments.GetQuotes(asset, dateRanges) => getQuotes(asset, dateRanges, inputFiles)
    }
  }

  private def findVariation(asset: String, minVariation: Double, filePaths: Seq[InputFile[QuotesFileName]]): Unit = {
    val dateQuotes = readAssetQuotes(asset, filePaths)

    if (dateQuotes.isEmpty) {
      println(s"Nenhuma cotação encontrada para $asset!")
    } else {
      for {
        (label, date, quote) <- Seq(
          ("inicial", dateQuotes.head.date, dateQuotes.head.quote),
          ("final  ", dateQuotes.last.date, dateQuotes.last.quote),
        )
      } {
        println(s"Cotação $label ($date): ${BrNumber.formatMoney(quote)}")
      }

      for {
        Seq(dq1, dq2) <- dateQuotes.sliding(2)
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

  //noinspection AccessorLikeMethodIsUnit
  private def getQuotes(asset: String, dateRanges: DateRanges, filePaths: Seq[InputFile[QuotesFileName]]): Unit = {
    val dateQuotes = readAssetQuotes(asset, filePaths)
    for {
      dateRange <- dateRanges.indexedSeq
      date <- dateRange
    } {
      PredicateBinarySearch.search(date, dateQuotes)(_.date) match {
        case PredicateBinarySearch.Found(index) =>
          val dateQuote = dateQuotes(index)
          assert(dateQuote.date == date)
          println(s"$date: ${BrNumber.formatMoney(dateQuote.quote)}")
        case PredicateBinarySearch.NotFound(0) =>
          println(s"$date: Data anterior aos dados no(s) arquivo(s) de cotações")
        case PredicateBinarySearch.NotFound(insertionIndex) if insertionIndex >= dateQuotes.length =>
          println(s"$date: Data posterior aos dados no(s) arquivo(s) de cotações")
        case PredicateBinarySearch.NotFound(insertionIndex) =>
          val dateQuote = dateQuotes(insertionIndex - 1)
          println(s"$date: ${BrNumber.formatMoney(dateQuote.quote)} (cotação de ${dateQuote.date})")
      }
    }
  }

  private def readAssetQuotes(asset: String, filePaths: Seq[InputFile[QuotesFileName]]): IndexedSeq[DateQuote] = {
    val dateQuotes =
      for {
        filePath <- filePaths
        dateQuote <- filePath.path.read { inputStream =>
          readAssetQuotes(asset, inputStream)
        }
      } yield dateQuote

    dateQuotes.groupMapReduce(_.date)(_.quote)((q, _) => q)
      .map(DateQuote.tupled)
      .toIndexedSeq
      .sortBy(_.date)
  }

  private def readAssetQuotes(asset: String, inputStream: InputStream): Seq[DateQuote] =
    (
      for {
        record <- QuotesFileReader.readFrom(inputStream)
        if record.asset == asset
      } yield DateQuote(record.date, record.closePrice)
    ).toSeq
}
