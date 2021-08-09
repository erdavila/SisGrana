package sisgrana
package investments.variableIncome.incomeRate

import investments.variableIncome.model.AssetPeriodTest.DSL._
import investments.variableIncome.model.{Persisted, StockbrokerAsset}
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
      "filter" -> "expected portfolio",
      AssetFilter() -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(0)) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(1)) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 10)),
        portfolioEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(minDate = Some(5)) -> Map(
        portfolioEntry("asset-1", "stockbroker-2")((5, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((5, 10)),
        portfolioEntry("asset-2", "stockbroker-1")((5, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((5, 10)),
      ),
      AssetFilter(minDate = Some(10)) -> Map.empty,
      AssetFilter(minDate = Some(11)) -> Map.empty,
      AssetFilter(maxDate = Some(5)) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 5)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 5)),
      ),
      AssetFilter(asset = Some("asset-1")) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-2")((1, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), minDate = Some(5)) -> Map(
        portfolioEntry("asset-1", "stockbroker-2")((5, 10)),
        portfolioEntry("asset-1", "stockbroker-3")((5, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), maxDate = Some(5)) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioEntry("asset-1", "stockbroker-3")((1, 5)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1")) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-2", "stockbroker-1")((1, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), minDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((5, 10)),
        portfolioEntry("asset-3", "stockbroker-1")((5, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), maxDate = Some(5)) -> Map(
        portfolioEntry("asset-1", "stockbroker-1")((1, 5)),
        portfolioEntry("asset-3", "stockbroker-1")((1, 5)),
      ),
      AssetFilter(asset = Some("asset-1"), stockbroker = Some("stockbroker-2")) -> Map(
        portfolioEntry("asset-1", "stockbroker-2")((1, 10)),
      ),
    )

    forAll(cases) { case (filter, expectedPortfolio) =>
      val portfolio = filtersResolver.resolveFilter(filter)
      portfolio should equal (expectedPortfolio)
    }
  }

  test(".applyNegativeFilter()") {
    val portfolio = Map(
      portfolioEntry("asset-2", "stockbroker-1")((1, 8)),
      portfolioEntry("asset-1", "stockbroker-1")((2, 9)),
      portfolioEntry("asset-1", "stockbroker-2")((3, 10)),
    )

    val cases = Table(
      "filter" -> "expected Portfolio",
      AssetFilter(minDate = Some(0)) -> Map.empty,
      AssetFilter(minDate = Some(1)) -> Map.empty,
      AssetFilter(minDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 5)),
        portfolioEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioEntry("asset-1", "stockbroker-2")((3, 5)),
      ),
      AssetFilter(minDate = Some(10)) -> portfolio,
      AssetFilter(minDate = Some(11)) -> portfolio,
      AssetFilter(maxDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((5, 8)),
        portfolioEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioEntry("asset-1", "stockbroker-2")((5, 10)),
      ),
      AssetFilter(asset = Some("asset-1")) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 8)),
      ),
      AssetFilter(asset = Some("asset-1"), minDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioEntry("asset-1", "stockbroker-2")((3, 5)),
      ),
      AssetFilter(asset = Some("asset-1"), maxDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioEntry("asset-1", "stockbroker-2")((5, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1")) -> Map(
        portfolioEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), minDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 5)),
        portfolioEntry("asset-1", "stockbroker-1")((2, 5)),
        portfolioEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(stockbroker = Some("stockbroker-1"), maxDate = Some(5)) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((5, 8)),
        portfolioEntry("asset-1", "stockbroker-1")((5, 9)),
        portfolioEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
      AssetFilter(asset = Some("asset-1"), stockbroker = Some("stockbroker-1")) -> Map(
        portfolioEntry("asset-2", "stockbroker-1")((1, 8)),
        portfolioEntry("asset-1", "stockbroker-2")((3, 10)),
      ),
    )

    forAll(cases) { case (filter, expectedPortfolio) =>
      val result = filtersResolver.applyNegativeFilter(portfolio, filter)
      result should equal (expectedPortfolio)
    }
  }

  test(".mergePortfolios()") {
    val p1 = Map(
      portfolioEntry("a1", "s1")((1, 3)),
      portfolioEntry("a2", "s1")((2, 10)),
    )
    val p2 = Map(
      portfolioEntry("a1", "s1")((7, 10)),
      portfolioEntry("a1", "s2")((1, 5)),
    )

    val result = FiltersResolver.mergePortfolios(p1, p2)

    val expectedResult = Map(
      portfolioEntry("a1", "s1")((1, 3), (7, 10)),
      portfolioEntry("a2", "s1")((2, 10)),
      portfolioEntry("a1", "s2")((1, 5)),
    )

    result should equal (expectedResult)
  }

  private def portfolioEntry(asset: String, stockbroker: String)(dateRangeSeq: DateRange*): (StockbrokerAsset, DateRanges) =
    StockbrokerAsset(stockbroker, asset) -> DateRanges.from(dateRangeSeq)
}
