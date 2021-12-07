package sisgrana
package investments.commands.incomeRate

import investments.model.AssetPeriodTest.DSL._
import investments.model.{Persisted, StockbrokerAsset}
import utils.DateRange.Mode.DayChange
import utils.DateRangeTest.{int2Date, intPair2DateRange}
import utils.{DateRange, DateRanges}

class FiltersResolverTest extends TestBase with Persisted {
  private val period = Period.DateRange(1, 10)
  assert(period.beginDate == period.dateRange.beginDate) // no adjustment
  assert(period.endDate == period.dateRange.endDate) // no adjustment
  private val filtersResolver = new FiltersResolver(period, adjustDate = identity)

  test(".resolveFilter()") {
    persisted(
      assetPeriodsFor("asset-1", "stockbroker-1")(_
        .on(-2).resultingQuantity(NonZero)
        .on(3).resultingQuantity(0)
      ),
      assetPeriodsFor("asset-1", "stockbroker-2")(_
        .on(7).resultingQuantity(NonZero)
      ),
      assetPeriodsFor("asset-1", "stockbroker-3")(_
        .on(4).resultingQuantity(NonZero)
        .on(6).resultingQuantity(0)
      ),
      assetPeriodsFor("asset-1", "stockbroker-4")(_
        .on(11).resultingQuantity(NonZero)
      ),
      assetPeriodsFor("asset-1", "stockbroker-5")(_
        .on(-5).resultingQuantity(NonZero)
        .on(-3).resultingQuantity(0)
      ),
      assetPeriodsFor("asset-2", "stockbroker-1")(_
        .on(7).resultingQuantity(NonZero)
      ),
      assetPeriodsFor("asset-3", "stockbroker-1")(_
        .on(4).resultingQuantity(NonZero)
        .on(6).resultingQuantity(0)
      ),
      assetPeriodsFor("asset-4", "stockbroker-1")(_
        .on(11).resultingQuantity(NonZero)
      ),
      assetPeriodsFor("asset-5", "stockbroker-1")(_
        .on(-5).resultingQuantity(NonZero)
        .on(-3).resultingQuantity(0)
      ),
      assetPeriodsFor("asset-1", "stockbroker-X")(_
        .on(-3).resultingQuantity(NonZero)
        .on(1).resultingQuantity(0)
        .on(10).resultingQuantity(NonZero)
        .on(13).resultingQuantity(0)
      ),
    )

    val cases = Table(
      "filter" -> "expected portfolio content",
      AssetFilter() -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(0)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(1)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-2")((5, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((5, 10)),
        portfolioContentEntry("asset-2", "stockbroker-1")((5, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((5, 10)),
      ),
      AssetFilter(minDate = Some(10)) -> Map.empty,
      AssetFilter(minDate = Some(11)) -> Map.empty,
      AssetFilter(maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 5)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 5)),
      ),
      AssetFilter(asset = Some("asset-1")) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-2")((5, 10)),
        portfolioContentEntry("asset-1", "stockbroker-3")((5, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioContentEntry("asset-1", "stockbroker-3")((1, 5)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1")) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((5, 10)),
        portfolioContentEntry("asset-3", "stockbroker-1")((5, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioContentEntry("asset-3", "stockbroker-1")((1, 5)),
      ),
      AssetFilter(asset = Some("asset-1"), stockbroker = Some("stockbroker-2")) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-2")((1, 10)),
      ),
    )

    forAll(cases) { case (filter, expectedPortfolioContent) =>
      val portfolioContent = filtersResolver.resolveFilter(filter)
      portfolioContent should equal (expectedPortfolioContent)
    }
  }

  test(".applyNegativeFilter()") {
    val portfolioContent = Map(
      portfolioContentEntry("asset-2", "stockbroker-1")((1, 8)),
      portfolioContentEntry("asset-1", "stockbroker-1")((2, 9)),
      portfolioContentEntry("asset-1", "stockbroker-2")((3, 10)),
    )

    val cases = Table(
      "filter" -> "expected portfolio content",
      AssetFilter(minDate = Some(0)) -> Map.empty,
      AssetFilter(minDate = Some(1)) -> Map.empty,
      AssetFilter(minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 5)),
        portfolioContentEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 5)),
      ),
      AssetFilter(minDate = Some(10)) -> portfolioContent,
      AssetFilter(minDate = Some(11)) -> portfolioContent,
      AssetFilter(maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((5, 8)),
        portfolioContentEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioContentEntry("asset-1", "stockbroker-2")((5, 10)),
      ),
      AssetFilter(asset = Some("asset-1")) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 8)),
      ),
      AssetFilter(asset = Some("asset-1"), minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioContentEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 5)),
      ),
      AssetFilter(asset = Some("asset-1"), maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioContentEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioContentEntry("asset-1", "stockbroker-2")((5, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1")) -> Map(
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), minDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 5)),
        portfolioContentEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), maxDate = Some(5)) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((5, 8)),
        portfolioContentEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), stockbroker = Some("stockbroker-1")) -> Map(
        portfolioContentEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioContentEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
    )

    forAll(cases) { case (filter, expectedPortfolioContent) =>
      val result = filtersResolver.applyNegativeFilter(portfolioContent, filter)
      result should equal (expectedPortfolioContent)
    }
  }

  test(".mergePortfolios()") {
    val p1 = Map(
      portfolioContentEntry("a1", "s1")((1, 3)),
      portfolioContentEntry("a2", "s1")((2, 10)),
    )
    val p2 = Map(
      portfolioContentEntry("a1", "s1")((7, 10)),
      portfolioContentEntry("a1", "s2")((1, 5)),
    )

    val result = FiltersResolver.mergePortfolioContents(p1, p2)

    val expectedResult = Map(
      portfolioContentEntry("a1", "s1")((1, 3), (7, 10)),
      portfolioContentEntry("a2", "s1")((2, 10)),
      portfolioContentEntry("a1", "s2")((1, 5)),
    )

    result should equal (expectedResult)
  }

  private def portfolioContentEntry(asset: String, stockbroker: String)(dateRangeSeq: DateRange*): (StockbrokerAsset, DateRanges) =
    StockbrokerAsset(stockbroker, asset) -> DateRanges.from(dateRangeSeq)
}
