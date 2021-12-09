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
    (dateRange(filter), filter.portfolio) match {
      case (None, _) => Map.empty
      case (Some(dateRange), None) =>
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
      case (Some(dateRange), Some(_)) =>
        val periodDateRanges = DateRanges.single(dateRange)
        portfolioContentFromFilter(filter) match {
          case Some(portfolioContent) =>
            portfolioContent
              .view
              .mapValues(_ `intersect` periodDateRanges)
              .filter { case (_, dateRanges) => dateRanges.nonEmpty }
              .toMap
          case None => Map.empty
        }
    }

  private[incomeRate] def applyNegativeFilter(portfolioContent: PortfolioContent, filter: AssetFilter): PortfolioContent = {
    require(
      Seq(
        filter.asset,
        filter.stockbroker,
        filter.portfolio,
        filter.minDate,
        filter.maxDate,
      ).exists(_.nonEmpty)
    )

    (dateRange(filter), filter.portfolio) match {
      case (None, _) => portfolioContent
      case (Some(dateRange), None) =>
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
      case (_, Some(_)) =>
        portfolioContentFromFilter(filter) match {
          case Some(filterPortfolioContent) =>
            portfolioContent.flatMap { case (stockbrokerAsset, dateRanges) =>
              filterPortfolioContent.get(stockbrokerAsset) match {
                case Some(filterDateRanges) =>
                  val resultDateRanges = dateRanges `diff` filterDateRanges
                  Option.when(resultDateRanges.nonEmpty)(stockbrokerAsset -> resultDateRanges)
                case None => Some(stockbrokerAsset -> dateRanges)
              }
            }
          case None => portfolioContent
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

  private def portfolioContentFromFilter(filter: AssetFilter): Option[PortfolioContent] = {
    require(filter.portfolio.isDefined)
    for (portfolio <- Portfolio.load(filter.portfolio.get))
      yield portfolio.content
        .pipeWhenMatched(filter.asset) { case Some(asset) => _.filter(_._1.asset == asset) }
        .pipeWhenMatched(filter.stockbroker) { case Some(stockbroker) => _.filter(_._1.stockbroker == stockbroker) }
        .pipeWhenMatched(filter.minDate) { case Some(minDate) => _.view.mapValues(_ `intersect` DateRanges.single(minDate, LocalDate.MAX)).toMap }
        .pipeWhenMatched(filter.maxDate) { case Some(maxDate) => _.view.mapValues(_ `intersect` DateRanges.single(LocalDate.MIN, maxDate)).toMap }
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
