package sisgrana
package investments.variableIncome.importQuotes

import investments.variableIncome.model.{AssetChange, PurchaseAmountWithCost}
import java.time.LocalDate
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class AssetDateRangeTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  test("seqFromAssetChanges") {
    val DateBefore = D(-1)
    val MinDate = D(0)
    val DateInRange1 = D(1)
    val DateInRange2 = D(2)
    val DateInRange3 = D(3)
    val MaxDate = D(4)

    val cases = Table(
      ("changes", "expected ranges"),

      changes() -> ranges(),

      changes((DateBefore, 0)) -> ranges(),
      changes((DateBefore, 999)) -> ranges((MinDate, MaxDate)),
      changes((DateInRange2, 0)) -> ranges(),
      changes((DateInRange2, 999)) -> ranges((DateInRange2, MaxDate)),

      changes((DateBefore, 0), (DateInRange2, 0)) -> ranges(),
      changes((DateBefore, 0), (DateInRange2, 999)) -> ranges((DateInRange2, MaxDate)),
      changes((DateBefore, 999), (DateInRange2, 0)) -> ranges((MinDate, DateInRange2)),
      changes((DateBefore, 999), (DateInRange2, 999)) -> ranges((MinDate, MaxDate)),
      changes((DateInRange1, 0), (DateInRange3, 0)) -> ranges(),
      changes((DateInRange1, 0), (DateInRange3, 999)) -> ranges((DateInRange3, MaxDate)),
      changes((DateInRange1, 999), (DateInRange3, 0)) -> ranges((DateInRange1, DateInRange3)),
      changes((DateInRange1, 999), (DateInRange3, 999)) -> ranges((DateInRange1, MaxDate)),

      changes((DateBefore, 0), (DateInRange1, 0), (DateInRange3, 0)) -> ranges(),
      changes((DateBefore, 0), (DateInRange1, 0), (DateInRange3, 999)) -> ranges((DateInRange3, MaxDate)),
      changes((DateBefore, 0), (DateInRange1, 999), (DateInRange3, 0)) -> ranges((DateInRange1, DateInRange3)),
      changes((DateBefore, 0), (DateInRange1, 999), (DateInRange3, 999)) -> ranges((DateInRange1, MaxDate)),
      changes((DateBefore, 999), (DateInRange1, 0), (DateInRange3, 0)) -> ranges((MinDate, DateInRange1)),
      changes((DateBefore, 999), (DateInRange1, 0), (DateInRange3, 999)) -> ranges((MinDate, DateInRange1), (DateInRange3, MaxDate)),
      changes((DateBefore, 999), (DateInRange1, 999), (DateInRange3, 0)) -> ranges((MinDate, DateInRange3)),
      changes((DateBefore, 999), (DateInRange1, 999), (DateInRange3, 999)) -> ranges((MinDate, MaxDate)),
      changes((DateInRange1, 0), (DateInRange2, 0), (DateInRange3, 0)) -> ranges(),
      changes((DateInRange1, 0), (DateInRange2, 0), (DateInRange3, 999)) -> ranges((DateInRange3, MaxDate)),
      changes((DateInRange1, 0), (DateInRange2, 999), (DateInRange3, 0)) -> ranges((DateInRange2, DateInRange3)),
      changes((DateInRange1, 0), (DateInRange2, 999), (DateInRange3, 999)) -> ranges((DateInRange2, MaxDate)),
      changes((DateInRange1, 999), (DateInRange2, 0), (DateInRange3, 0)) -> ranges((DateInRange1, DateInRange2)),
      changes((DateInRange1, 999), (DateInRange2, 0), (DateInRange3, 999)) -> ranges((DateInRange1, DateInRange2), (DateInRange3, MaxDate)),
      changes((DateInRange1, 999), (DateInRange2, 999), (DateInRange3, 0)) -> ranges((DateInRange1, DateInRange3)),
      changes((DateInRange1, 999), (DateInRange2, 999), (DateInRange3, 999)) -> ranges((DateInRange1, MaxDate)),
    )

    forAll(cases) { (changes, ranges) =>
      AssetDateRange.seqFromAssetChanges(changes, MinDate, MaxDate) should equal (ranges)
    }
  }

  test("mergeSeqs") {
    val cases = Table(
      ("seqs", "expected"),
      (
        ranges(),
        ranges(),
      ) ->
        ranges()
      ,
      (
        ranges((D(0),/*=*/D(1))),
        ranges(                ),
      ) ->
        ranges((D(0),/*=*/D(1)))
      ,
      (
        ranges((D(0),/*=*/D(1))),
        ranges((D(0),/*=*/D(1))),
      ) ->
        ranges((D(0),/*=*/D(1)))
      ,
      (
        ranges((D(0),/*=*/D(1))          ),
        ranges((D(0),/*===========*/D(2))),
      ) ->
        ranges((D(0),/*===========*/D(2)))
      ,
      (
        ranges((D(0),/*===========*/D(2))),
        ranges(          (D(1),/*=*/D(2))),
      ) ->
        ranges((D(0),/*===========*/D(2)))
      ,
      (
        ranges((D(0),/*===========*/D(2))          ),
        ranges(          (D(1),/*===========*/D(3))),
      ) ->
        ranges((D(0),/*=====================*/D(3)))
      ,
      (
        ranges((D(0),/*=====================*/D(3))),
        ranges(          (D(1),/*=*/D(2))          ),
      ) ->
        ranges((D(0),/*=====================*/D(3)))
      ,
      (
        ranges((D(0),/*===============================*/D(4))          ),
        ranges(          (D(1),/*=*/D(2)),   (D(3),/*===========*/D(5))),
      ) ->
        ranges((D(0),/*=========================================*/D(5)))
      ,
      (
        ranges((D(0),/*=====================*/D(3)),   (D(4),/*=*/D(5))),
        ranges(          (D(1),/*=*/D(2))                              ),
      ) ->
        ranges((D(0),/*=====================*/D(3)),   (D(4),/*=*/D(5)))
      ,
      (
        ranges((D(0),/*=====================*/D(3))                    ),
        ranges(          (D(1),/*=*/D(2)),             (D(4),/*=*/D(5))),
      ) ->
        ranges((D(0),/*=====================*/D(3)),   (D(4),/*=*/D(5)))
      ,
      (
        ranges((D(0),/*=====================*/D(3)),   (D(4),/*=*/D(5))          ),
        ranges(          (D(1),/*=*/D(2)),                       (D(5),/*=*/D(6))),
      ) ->
        ranges((D(0),/*=====================*/D(3)),   (D(4),/*===========*/D(6)))
      ,
    )

    forAll(cases) { case ((seq1, seq2), expected) =>
      AssetDateRange.mergeSeqs(seq1, seq2) should equal (expected)
      AssetDateRange.mergeSeqs(seq2, seq1) should equal (expected)
    }
  }

  private def D(i: Int): LocalDate = LocalDate.of(2021, 1, 1).plusDays(i)

  private def changes(tuple: (LocalDate, Int)*): Seq[AssetChange] =
    for ((date, positionQuantity) <- tuple)
      yield AssetChange.withZeroes("", "", date)
        .withPostEventPosition(PurchaseAmountWithCost.fromTotals(positionQuantity, positionQuantity.toDouble, 0.0))

  private def ranges(dates: (LocalDate, LocalDate)*): Seq[AssetDateRange] =
    for ((beginDate, endDate) <- dates)
      yield AssetDateRange(beginDate, endDate)
}
