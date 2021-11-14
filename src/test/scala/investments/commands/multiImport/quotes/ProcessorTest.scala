package sisgrana
package investments.commands.multiImport.quotes

import investments.model.AssetPeriodTest.DSL._
import investments.model.Persisted
import utils.DateRange.Mode.FullDay
import utils.DateRangeTest.int2Date
import utils.{DateRange, DateRanges}

class ProcessorTest extends TestBase with Persisted {
  test(".makeAssetsDateRanges()") {
    val MaxDate = 10

    def ranges(pairs: (Int, Int)*): DateRanges =
      DateRanges.from(
        for ((b, e) <- pairs)
          yield DateRange(b, e)
      )

    val cases = Table(
      "asset periods" -> "expected date ranges",
      Seq(
        assetPeriodsFor("asset-1", "stockbrokerA")(_
          .on(2).resultingQuantity(NonZero)
          .on(4).resultingQuantity(0)
        ),
        assetPeriodsFor("asset-1", "stockbrokerB")(_
          .on(3).resultingQuantity(NonZero)
          .on(5).resultingQuantity(0)
        ),
        assetPeriodsFor("asset-2")(_
          .on(3).resultingQuantity(NonZero)
          .on(6).resultingQuantity(0)
        ),
      ).flatten -> Map(
        "asset-1" -> ranges((2, 5)),
        "asset-2" -> ranges((3, 6)),
      ),
      Seq(
        assetPeriodsFor("asset")(_
          .on(1).resultingQuantity(NonZero)
          .on(2).eventConvertedToOther("other-asset")
        ),
        assetPeriodsFor("other-asset")(_
          .on(2).eventIncreasedQuantity(NonZero)
        )
      ).flatten -> Map(
        "asset" -> ranges((1, 2)),
        "other-asset" -> ranges((2, MaxDate)),
      ),
      Seq(
        assetPeriodsFor("asset")(_
          .on(1).resultingQuantity(NonZero)
          .on(2).eventConvertedToOther("other-asset")
        ),
        assetPeriodsFor("other-asset")(_
          .on(2).eventIncreasedQuantity(NonZero).resultingQuantity(0)
        )
      ).flatten -> Map(
        "asset" -> ranges((1, 2)),
        "other-asset" -> ranges((2, 2)),
      ),
    )

    forAll(cases) { (assetPeriods, expectedDateRanges) =>
      persisted(assetPeriods)

      val result = Processor.makeAssetsDateRanges(minDate = 0, MaxDate)
      result should equal (expectedDateRanges)
    }
  }
}
