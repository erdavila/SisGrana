package sisgrana
package investments.commands.incomeRate

import investments.commands.incomeRate.FiltersResolver.mergePortfolioContents
import investments.model._
import investments.model.ctx.{localDateDecoder => _, localDateEncoder => _, _}
import java.time.LocalDate
import utils.DateRange.Mode.DayChange
import utils.{DateRange, DateRanges}

class FiltersResolver(
  period: Period,
  adjustDate: LocalDate => LocalDate = NonQuoteDate.adjustToQuoteDate
) extends LocalDateSupport {
  def resolve(positiveFilters: Seq[AssetFilter], negativeFilters: Seq[AssetFilter]): PortfolioContent = {
    val positivePortfolioContent = positiveFilters
      .map(resolveFilter)
      .reduce(mergePortfolioContents)

    negativeFilters.foldLeft(positivePortfolioContent)(applyNegativeFilter)
  }

  private[incomeRate] def resolveFilter(filter: AssetFilter): PortfolioContent =
    dateRange(filter) match {
      case None => Map.empty
      case Some(dateRange) =>
        val assetPeriods = ctx.run(
          //noinspection ComparingUnrelatedTypes
          AssetPeriod.inDateRangeQuery(dateRange)
            .dynamic
            .filter(_.resultingPositionQuantity > 0)
            .filterOpt(filter.asset)((ap, asset) => quote(ap.asset == asset))
            .filterOpt(filter.stockbroker)((ap, stockbroker) => quote(ap.stockbroker == stockbroker))
        )

        val periodDateRanges = DateRanges.single(dateRange)

        assetPeriods
          .groupMap(_.stockbrokerAsset)(_.dateRange)
          .view
          .mapValues(drs => DateRanges.from(drs) `intersect` periodDateRanges)
          .filter(_._2.nonEmpty)
          .mapValues(_ => periodDateRanges)
          .to(Map)
    }

  private[incomeRate] def applyNegativeFilter(portfolioContent: PortfolioContent, filter: AssetFilter): PortfolioContent = {
    require(
      Seq(
        filter.asset,
        filter.stockbroker,
        filter.minDate,
        filter.maxDate,
      ).exists(_.nonEmpty)
    )

    dateRange(filter) match {
      case None => portfolioContent
      case Some(dateRange) =>
        val predicate: StockbrokerAsset => Boolean =
          (filter.asset, filter.stockbroker) match {
            case (Some(asset), Some(stockbroker)) =>
              stockbrokerAsset => stockbrokerAsset.asset == asset && stockbrokerAsset.stockbroker == stockbroker
            case (Some(asset), None) => _.asset == asset
            case (None, Some(stockbroker)) => _.stockbroker == stockbroker
            case (None, None) => _ => true
          }

        val periodDateRanges = DateRanges.single(dateRange)

        portfolioContent
          .flatMap { case (stockbrokerAsset, dateRanges) =>
            if (predicate(stockbrokerAsset)) {
              val newDateRanges = dateRanges `diff` periodDateRanges
              Option.when(newDateRanges.nonEmpty) {
                stockbrokerAsset -> newDateRanges
              }
            } else {
              Some(stockbrokerAsset -> dateRanges)
            }
          }
    }
  }

  private def dateRange(filter: AssetFilter): Option[DateRange] = {
    val minDate = (filter.minDate.map(adjustDate) ++ Some(period.dateRange.beginDate)).max
    val maxDate = (filter.maxDate.map(adjustDate) ++ Some(period.dateRange.endDate)).min

    Option.when(!(minDate `isAfter` maxDate)) {
      DateRange(minDate, maxDate)
    }
  }
}

object FiltersResolver {
  private[incomeRate] def mergePortfolioContents(p1: PortfolioContent, p2: PortfolioContent): PortfolioContent =
    (p1 `foldLeft` p2) { case (p2, (stockbrokerAsset1, dateRanges1)) =>
      p2.updatedWith(stockbrokerAsset1) {
        case Some(dateRanges2) => Some(dateRanges1 `union` dateRanges2)
        case None => Some(dateRanges1)
      }
    }
}
