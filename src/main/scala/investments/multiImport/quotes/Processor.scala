package sisgrana
package investments.multiImport.quotes

import investments.QuotesFileReader
import investments.files.InputFile
import investments.model.LocalDateSupport._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import investments.model.{AssetPeriod, AssetQuote, ctx}
import investments.quotesFiles.QuotesFileName
import java.io.InputStream
import java.time.{LocalDate, Month}
import utils.DateRange.Mode.FullDay
import utils.{DateRange, DateRanges}

object Processor {
  private val AcceptedAssetVariations = Set("3", "4", "5", "6", "11")

  def process(inputFiles: Seq[InputFile[QuotesFileName]]): Unit =
    for (inputFile <- inputFiles) {
      processFile(inputFile)
    }

  private def processFile(inputFile: InputFile[QuotesFileName]): Unit = {
    val (minDate, maxDate) = inputFile.name.period match {
      case QuotesFileName.Year(year) => (year.atDay(1), year.atMonth(Month.DECEMBER).atEndOfMonth())
      case QuotesFileName.Month(month) => (month.atDay(1), month.atEndOfMonth())
      case QuotesFileName.Date(date) => (date, date)
    }

    println(s"Importando cotações em ${inputFile.path.stringPath}")
    inputFile.path.read { inputStream =>
      importQuotes(minDate, maxDate, inputStream)
    }
  }

  private def importQuotes(minDate: LocalDate, maxDate: LocalDate, inputStream: InputStream): Unit = {
    val assetsDateRanges = makeAssetsDateRanges(minDate, maxDate)

    val quotesIterator =
      for {
        record <- QuotesFileReader.readFrom(inputStream)
        if AcceptedAssetVariations.contains(record.asset.substring(4))
        dateRanges <- assetsDateRanges.get(record.asset)
        if dateRanges.contains(record.date)
      } yield AssetQuote(
        record.asset,
        record.date,
        record.openPrice,
        record.closePrice,
        record.minPrice,
        record.avgPrice,
        record.maxPrice,
      )

    val quotes = quotesIterator.toSeq
    println(s"  ${quotes.length} cotações")

    ctx.transaction(
      ctx.run(
        for (quote <- liftQuery(quotes)) {
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

  private[quotes] def makeAssetsDateRanges(minDate: LocalDate, maxDate: LocalDate): Map[String, DateRanges] = {
    val assetPeriods =
      ctx.run(
        AssetPeriod.betweenDatesQuery(minDate, maxDate)
          .filter(_.resultingPositionQuantity != 0)
      )

    val dateRanges = DateRanges.single(DateRange(minDate, maxDate))

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
}
