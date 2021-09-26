package sisgrana
package investments.variableIncome.importQuotes

import investments.variableIncome.model._
import investments.variableIncome.model.ctx._
import java.io.InputStream
import java.time.{LocalDate, Month, Year, YearMonth}
import java.util.zip.ZipInputStream
import scala.io.Source
import utils.DateRange.Mode.FullDay
import utils.{DateRange, DateRanges, IndentedPrinter}

class FilesProcessor extends LocalDateSupport {
  private val printer = new IndentedPrinter

  private object YearFileName {
    private val Regex = """COTAHIST_A(\d{4})\.TXT""".r
    def unapply(fileName: String): Option[Year] =
      fileName match {
        case Regex(yearString) => Some(Year.parse(yearString))
        case _ => None
      }
  }

  private object MonthFileName {
    private val Regex = """COTAHIST_M(\d{2})(\d{4})\.TXT""".r
    def unapply(fileName: String): Option[YearMonth] =
      fileName match {
        case Regex(monthString, yearString) => Some(YearMonth.parse(s"$yearString-$monthString"))
        case _ => None
      }
  }

  private object DayFileName {
    private val Regex = """COTAHIST_D(\d{2})(\d{2})(\d{4}).TXT""".r
    def unapply(fileName: String): Option[LocalDate] =
      fileName match {
        case Regex(dayString, monthString, yearString) => Some(LocalDate.parse(s"$yearString-$monthString-$dayString"))
        case _ => None
      }
  }

  private val AcceptedAssetVariations = Set("3", "4", "5", "6", "11")

  private val DateExtractor = new StringExtractor(3, 10, str => {
    val yearString = str.substring(0, 4)
    val monthString = str.substring(4, 6)
    val dayString = str.substring(6, 8)
    val dateString = yearString + "-" + monthString + "-" + dayString
    LocalDate.parse(dateString)
  })

  private val AssetExtractor = new StringExtractor(13, 24, _.trim)
  private val OpenPriceExtractor = new StringExtractor.PriceExtractor(57)
  private val MaxPriceExtractor = new StringExtractor.PriceExtractor(70)
  private val MinPriceExtractor = new StringExtractor.PriceExtractor(83)
  private val AvgPriceExtractor = new StringExtractor.PriceExtractor(96)
  private val ClosePriceExtractor = new StringExtractor.PriceExtractor(109)

  def processFile(multiFile: MultiFile, inputStream: => InputStream): Unit =
    multiFile.name match {
      case YearFileName(year) => processYearQuotesFile(multiFile, year, inputStream)
      case MonthFileName(yearMonth) => processMonthQuotesFile(multiFile, yearMonth, inputStream)
      case DayFileName(date) => processDayQuotesFile(multiFile, date, inputStream)
      case name if name.toLowerCase.endsWith(".zip") => processZipFile(multiFile, inputStream)
      case _ => printer.println(s"Ignorando arquivo $multiFile")
    }
  private def processYearQuotesFile(multiFile: MultiFile, year: Year, inputStream: InputStream): Unit =
    printer.context(s"Processando cotações do ano $year no arquivo $multiFile") {
      importQuotes(
        minDate = year.atDay(1),
        maxDate = year.atMonth(Month.DECEMBER).atEndOfMonth(),
        inputStream,
      )
    }

  private def processMonthQuotesFile(multiFile: MultiFile, yearMonth: YearMonth, inputStream: InputStream): Unit =
    printer.context(s"Processando cotações do mês $yearMonth no arquivo $multiFile") {
      importQuotes(
        minDate = yearMonth.atDay(1),
        maxDate = yearMonth.atEndOfMonth(),
        inputStream,
      )
    }

  private def processDayQuotesFile(multiFile: MultiFile, date: LocalDate, inputStream: InputStream): Unit =
    printer.context(s"Processando cotações do dia $date no arquivo $multiFile") {
      importQuotes(date, date, inputStream)
    }

  private def importQuotes(minDate: LocalDate, maxDate: LocalDate, inputStream: InputStream): Unit = {
    val assetsDateRanges = makeAssetsDateRanges(minDate, maxDate)

    val quotes =
      for {
        line <- Source.fromInputStream(inputStream).getLines()
        if line.startsWith("01")
        asset = AssetExtractor.from(line)
        if AcceptedAssetVariations.contains(asset.substring(4))
        dateRanges <- assetsDateRanges.get(asset)
        date = DateExtractor.from(line)
        if dateRanges.contains(date)
        openPrice = OpenPriceExtractor.from(line)
        closePrice = ClosePriceExtractor.from(line)
        minPrice = MinPriceExtractor.from(line)
        avgPrice = AvgPriceExtractor.from(line)
        maxPrice = MaxPriceExtractor.from(line)
      } yield AssetQuote(asset, date, openPrice, closePrice, minPrice, avgPrice, maxPrice)

    printer.println(s"${quotes.length} cotações")

    ctx.transaction(
      ctx.run(
        for (quote <- liftQuery(quotes.toSeq)) {
          query[AssetQuote]
            .insert(quote)
            .onConflictUpdate(_.asset, _.date)(
              (t, e) => t.openPrice -> e.openPrice,
              (t, e) => t.closePrice -> e.closePrice,
              (t, e) => t.minPrice -> e.minPrice,
              (t, e) => t.avgPrice -> e.avgPrice,
              (t, e) => t.maxPrice -> e.maxPrice,
            )
        }
      )
    )
  }

  private[importQuotes] def makeAssetsDateRanges(minDate: LocalDate, maxDate: LocalDate): Map[String, DateRanges] = {
    val assetPeriods =
      ctx.run(
        AssetPeriod.betweenDatesQuery(minDate, maxDate)
          .filter(_.resultingPositionQuantity != 0)
      )

    val dateRanges = DateRanges.from(Seq(DateRange(minDate, maxDate)))

    assetPeriods
      .flatMap { ap =>
        val convertedToAsset =
          for (ct <- ap.convertedTo)
            yield ct.asset -> DateRange(ap.endDate, ap.endDate)
        Some(ap.asset -> ap.dateRange) ++ convertedToAsset
      }
      .groupMap(_._1)(_._2)
      .view.mapValues(drs => DateRanges.from(drs) `intersect` dateRanges)
      .to(Map)
  }

  private def processZipFile(multiFile: MultiFile, inputStream: InputStream): Unit =
    printer.context(s"Processando arquivo ZIP $multiFile") {
      val stream = new ZipInputStream(inputStream)
      var entry = stream.getNextEntry
      while (entry != null) {
        if (!entry.isDirectory) {
          processFile(multiFile / entry.getName, stream)
        }
        entry = stream.getNextEntry
      }
    }
}
