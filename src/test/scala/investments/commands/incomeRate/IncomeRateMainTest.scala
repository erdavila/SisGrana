package sisgrana
package investments.commands.incomeRate

import TestBase.{DefaultAsset, DefaultStockbroker, DefaultStockbrokerAsset}
import investments.commands.incomeRate.IncomeRateMain.Position
import investments.model.AssetPeriodTest.DSL._
import investments.model.{ConvertedTo, Persisted, StockbrokerAsset}
import utils.DateRange
import utils.DateRangeTest.int2Date

class IncomeRateMainTest extends TestBase with Persisted {
  test(".queryQuantities()") {
    val cases = Table(
      "asset periods" -> "expected results",

      Seq.empty -> Seq.empty,

      assetPeriodsFor()(_
        .on(1).resultingQuantity(15)
        .on(3).resultingQuantity(0)
        .on(5).resultingQuantity(-5)
        .on(7).resultingQuantity(10)
        .on(9).resultingQuantity(0)
      ) -> Seq(
        DateRange(1, 3) -> position(15),
        DateRange(7, 9) -> position(10),
      ),

      assetPeriodsFor()(_
        .on(-2).resultingQuantity(10)
        .on(1).resultingQuantity(0)
        .on(10).resultingQuantity(15)
      ) -> Seq.empty,

      assetPeriodsFor()(_
        .on(-2).resultingQuantity(15)
      ) -> Seq(
        DateRange(1, 10) -> position(15),
      ),

      assetPeriodsFor()(_
        .on(2).resultingQuantity(10)
        .on(4).eventConvertedToSame(20)
        .on(6).resultingQuantity(15)
        .on(8).eventConvertedToOther("other-asset")
      ) -> Seq(
        DateRange(2, 4) -> position(10, ConvertedTo(DefaultAsset, 20.0)),
        DateRange(4, 6) -> position(20),
        DateRange(6, 8) -> position(15, ConvertedTo("other-asset", 15.0)),
      ),

      assetPeriodsFor()(_
        .on(5).resultingQuantity(10)
        .on(10).eventConvertedToSame(20)
      ) -> Seq(
        DateRange(5, 10) -> position(10, ConvertedTo(DefaultAsset, 20.0)),
      ),

      assetPeriodsFor()(_
        .on(-1).resultingQuantity(10)
        .on(4).eventConvertedToSame(20)
        .on(6).resultingQuantity(15)
        .on(12).eventConvertedToOther("other-asset")
      ) -> Seq(
        DateRange(1, 4) -> position(10, ConvertedTo(DefaultAsset, 20.0)),
        DateRange(4, 6) -> position(20),
        DateRange(6, 10) -> position(15), // No conversion
      ),
    )

    val dateRange = DateRange(1, 10)

    forAll(cases) { case assetPeriods -> expectedResults =>
      persisted(assetPeriods)

      val result = IncomeRateMain.queryQuantities(DefaultStockbrokerAsset, dateRange)

      result should equal (expectedResults)
    }
  }

  test(".regularizeDateRanges()") {
    def asset(a: String) = StockbrokerAsset(DefaultStockbroker, a)

    val cases = Table(
      "quantities" -> "expected results",

      Map(
        asset("a") -> Seq(
          DateRange(1, 5) -> position(10),
        ),
        asset("b") -> Seq(
          DateRange(1, 5) -> position(15),
        ),
      ) -> Seq(
        DateRange(1, 5) -> Map(
          asset("a") -> position(10),
          asset("b") -> position(15),
        ),
      ),

      Map(
        asset("a") -> Seq(
          DateRange(1, 5) -> position(10),
        ),
        asset("b") -> Seq(
          DateRange(3, 8) -> position(15),
        ),
      ) -> Seq(
        DateRange(1, 3) -> Map(
          asset("a") -> position(10),
        ),
        DateRange(3, 5) -> Map(
          asset("a") -> position(10),
          asset("b") -> position(15),
        ),
        DateRange(5, 8) -> Map(
          asset("b") -> position(15),
        ),
      ),

      Map(
        asset("a") -> Seq(
          DateRange(1, 3) -> position(10),
        ),
        asset("b") -> Seq(
          DateRange(5, 8) -> position(15),
        ),
      ) -> Seq(
        DateRange(1, 3) -> Map(
          asset("a") -> position(10),
        ),
        DateRange(5, 8) -> Map(
          asset("b") -> position(15),
        ),
      ),

      Map(
        asset("a") -> Seq(
          DateRange(1, 9) -> position(15),
        ),
        asset("b") -> Seq(
          DateRange(3, 5) -> position(10),
          DateRange(5, 7) -> position(20),
        ),
      ) -> Seq(
        DateRange(1, 3) -> Map(
          asset("a") -> position(15),
        ),
        DateRange(3, 5) -> Map(
          asset("a") -> position(15),
          asset("b") -> position(10),
        ),
        DateRange(5, 7) -> Map(
          asset("a") -> position(15),
          asset("b") -> position(20),
        ),
        DateRange(7, 9) -> Map(
          asset("a") -> position(15),
        ),
      ),

      Map(
        asset("a") -> Seq(
          DateRange(1, 5) -> position(10, ConvertedTo("a", 20.0)),
        ),
        asset("b1") -> Seq(
          DateRange(3, 8) -> position(15, ConvertedTo("b2", 30.0)),
        ),
      ) -> Seq(
        DateRange(1, 3) -> Map(
          asset("a") -> position(10),
        ),
        DateRange(3, 5) -> Map(
          asset("a") -> position(10, ConvertedTo("a", 20.0)),
          asset("b1") -> position(15),
        ),
        DateRange(5, 8) -> Map(
          asset("b1") -> position(15, ConvertedTo("b2", 30.0)),
        ),
      ),
    )

    forAll(cases) { case (quantities, expectedResults) =>
      val result = IncomeRateMain.regularizeDateRanges(quantities)

      result should equal (expectedResults)
    }
  }

  test(".joinSubPeriods()") {
    val data1to3 = DateRange(1, 3) -> 'A'
    val data3to6 = DateRange(3, 6) -> 'B'
    val data8to10 = DateRange(8, 10) -> 'C'

    val subPeriods = IncomeRateMain.joinSubPeriods(Seq(data1to3, data3to6, data8to10))
    subPeriods.map(_.data) should equal (
      Seq(
        Seq(data1to3, data3to6),
        Seq(data8to10),
      )
    )
  }

  test(".calculateNonDataSubPeriods()") {
    val periodDateRange = DateRange(1, 10)

    val cases = Table(
      "subPeriods" -> "expected nonDataSubPeriods",

      Seq(
        DateRange(1, 4) -> 'A',
        DateRange(6, 8) -> 'B',
        DateRange(9, 10) -> 'C',
      ) -> Seq(
        DateRange(4, 6),
        DateRange(8, 9),
      ),

      Seq(
        DateRange(3, 5) -> 'A',
        DateRange(6, 7) -> 'B',
      ) -> Seq(
        DateRange(1, 3),
        DateRange(5, 6),
        DateRange(7, 10),
      ),

      Seq.empty -> Seq(periodDateRange),
    )

    forAll(cases) { case (dataSubPeriods, expectedNonDataSubPeriods) =>
      val nonDataSubPeriods = IncomeRateMain.calculateNonDataSubPeriods(dataSubPeriods, periodDateRange)

      nonDataSubPeriods should equal (expectedNonDataSubPeriods)
    }
  }

  private def position(quantity: Int): Position =
    Position(quantity, None)

  private def position(quantity: Int, convertedTo: ConvertedTo): Position =
    Position(quantity, Some(convertedTo))
}
