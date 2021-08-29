package sisgrana
package investments.variableIncome.importQuotes

import investments.variableIncome.model.AssetChangeTest.DSL._
import utils.DateRange.Mode.FullDay
import utils.DateRangeTest.int2Date
import utils.{DateRange, DateRanges}

class FilesProcessorTest extends TestBase {
  test(".allAssetsChangesToDateRanges()") {
    val MaxDate = 10

    def ranges(pairs: (Int, Int)*): DateRanges =
      DateRanges.from(
        for ((b, e) <- pairs)
          yield DateRange(b, e)
      )

    val cases = Table(
      "asset changes" -> "expected date ranges",
      Seq(
        assetChangesFor("asset-1", "stockbrokerA")(_
          .on(2).resultingQuantity(NonZero)
          .on(4).resultingQuantity(0)
        ),
        assetChangesFor("asset-1", "stockbrokerB")(_
          .on(3).resultingQuantity(NonZero)
          .on(5).resultingQuantity(0)
        ),
        assetChangesFor("asset-2")(_
          .on(3).resultingQuantity(NonZero)
          .on(6).resultingQuantity(0)
        ),
      ).flatten -> Map(
        "asset-1" -> ranges((2, 5)),
        "asset-2" -> ranges((3, 6)),
      ),
      Seq(
        assetChangesFor("asset")(_
          .on(1).resultingQuantity(NonZero)
          .on(2).eventConvertedToOther("other-asset")
        ),
        assetChangesFor("other-asset")(_
          .on(2).eventIncreasedQuantity(NonZero)
        )
      ).flatten -> Map(
        "asset" -> ranges((1, 2)),
        "other-asset" -> ranges((2, MaxDate)),
      ),
      Seq(
        assetChangesFor("asset")(_
          .on(1).resultingQuantity(NonZero)
          .on(2).eventConvertedToOther("other-asset")
        ),
        assetChangesFor("other-asset")(_
          .on(2).eventIncreasedQuantity(NonZero).resultingQuantity(0)
        )
      ).flatten -> Map(
        "asset" -> ranges((1, 2)),
        "other-asset" -> ranges((2, 2)),
      ),
    )

    forAll(cases) { (assetChanges, expectedDateRanges) =>
      val result = FilesProcessor.allAssetsChangesToDateRanges(assetChanges, minDate = 0, MaxDate)
      result should equal (expectedDateRanges)
    }
  }
}
