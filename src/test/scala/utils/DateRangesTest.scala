package sisgrana
package utils

import utils.DateRangeTest.{int2Date, int2DateRange}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.{BeMatcher, MatchResult}
import org.scalatest.prop.TableDrivenPropertyChecks

class DateRangesTest extends AnyFunSuite with TableDrivenPropertyChecks with Matchers {
  import DateRangesTest.{IntOps, normalized}

  test(".from()") {
    val drSeq: Seq[DateRange] = Seq(
      /**/     1###2,
      /**/                                     9###10,
      /**/             3#######5,
      /**/ 0,
      /**/                             7###8,
      /**/         2#######4,
    )

    withClue("DayChange mode") {
      import DateRange.Mode.DayChange
      DateRanges.from(drSeq).indexedSeq should equal (
        Seq[DateRange](
          1###############5,      7###8,  9###10,
        )
      )
    }

    withClue("FullDay mode") {
      import DateRange.Mode.FullDay
      DateRanges.from(drSeq).indexedSeq should equal (
        Seq[DateRange](
          0###################5,      7###########10,
        )
      )
    }
  }

  test(".union()") {
    case class Case(
      seq1: Seq[DateRange],
      seq2: Seq[DateRange],
      expectedDayChangeUnion: Seq[DateRange],
      expectedFullDayUnion: Seq[DateRange],
    )

    val cases = Table(
      "case",
      Case(
        seq1 =                   Seq(),
        seq2 =                   Seq(),
        expectedDayChangeUnion = Seq(),
        expectedFullDayUnion =   Seq(),
      ),
      Case(
        seq1 =                   Seq( ),
        seq2 =                   Seq(0),
        expectedDayChangeUnion = Seq( ),
        expectedFullDayUnion =   Seq(0),
      ),
      Case(
        seq1 =                   Seq(     ),
        seq2 =                   Seq(0###1),
        expectedDayChangeUnion = Seq(0###1),
        expectedFullDayUnion =   Seq(0###1),
      ),
      Case(
        seq1 =                   Seq(0        ),
        seq2 =                   Seq(        2),
        expectedDayChangeUnion = Seq(         ),
        expectedFullDayUnion =   Seq(0,      2),
      ),
      Case(
        seq1 =                   Seq(0    ),
        seq2 =                   Seq(    1),
        expectedDayChangeUnion = Seq(     ),
        expectedFullDayUnion =   Seq(0###1),
      ),
      Case(
        seq1 =                   Seq(0),
        seq2 =                   Seq(0),
        expectedDayChangeUnion = Seq( ),
        expectedFullDayUnion =   Seq(0),
      ),
      Case(
        seq1 =                   Seq(0            ),
        seq2 =                   Seq(        2###3),
        expectedDayChangeUnion = Seq(        2###3),
        expectedFullDayUnion =   Seq(0,      2###3),
      ),
      Case(
        seq1 =                   Seq(0        ),
        seq2 =                   Seq(    1###2),
        expectedDayChangeUnion = Seq(    1###2),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(0    ),
        seq2 =                   Seq(0###1),
        expectedDayChangeUnion = Seq(0###1),
        expectedFullDayUnion =   Seq(0###1),
      ),
      Case(
        seq1 =                   Seq(    1    ),
        seq2 =                   Seq(0#######2),
        expectedDayChangeUnion = Seq(0#######2),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(    1),
        seq2 =                   Seq(0###1),
        expectedDayChangeUnion = Seq(0###1),
        expectedFullDayUnion =   Seq(0###1),
      ),
      Case(
        seq1 =                   Seq(        2),
        seq2 =                   Seq(0###1    ),
        expectedDayChangeUnion = Seq(0###1    ),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(            3),
        seq2 =                   Seq(0###1        ),
        expectedDayChangeUnion = Seq(0###1        ),
        expectedFullDayUnion =   Seq(0###1,      3),
      ),
      Case(
        seq1 =                   Seq(0###1            ),
        seq2 =                   Seq(            3###4),
        expectedDayChangeUnion = Seq(0###1,      3###4),
        expectedFullDayUnion =   Seq(0###1,      3###4),
      ),
      Case(
        seq1 =                   Seq(0###1        ),
        seq2 =                   Seq(        2###3),
        expectedDayChangeUnion = Seq(0###1,  2###3),
        expectedFullDayUnion =   Seq(0###########3),
      ),
      Case(
        seq1 =                   Seq(0###1    ),
        seq2 =                   Seq(    1###2),
        expectedDayChangeUnion = Seq(0#######2),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(0#######2    ),
        seq2 =                   Seq(    1#######3),
        expectedDayChangeUnion = Seq(0###########3),
        expectedFullDayUnion =   Seq(0###########3),
      ),
      Case(
        seq1 =                   Seq(0#######2),
        seq2 =                   Seq(    1###2),
        expectedDayChangeUnion = Seq(0#######2),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(0###########3),
        seq2 =                   Seq(    1###2    ),
        expectedDayChangeUnion = Seq(0###########3),
        expectedFullDayUnion =   Seq(0###########3),
      ),
      Case(
        seq1 =                   Seq(0###1    ),
        seq2 =                   Seq(0#######2),
        expectedDayChangeUnion = Seq(0#######2),
        expectedFullDayUnion =   Seq(0#######2),
      ),
      Case(
        seq1 =                   Seq(0###1),
        seq2 =                   Seq(0###1),
        expectedDayChangeUnion = Seq(0###1),
        expectedFullDayUnion =   Seq(0###1),
      ),
      Case(
        seq1 =                   Seq(0###1,      3#######5        ),
        seq2 =                   Seq(    1#######3,          6###7),
        expectedDayChangeUnion = Seq(0###################5,  6###7),
        expectedFullDayUnion =   Seq(0###########################7),
      ),
    )

    forAll(cases) { c =>
      def withMode(expectedUnion: Seq[DateRange])(implicit mode: DateRange.Mode): Unit =
        withClue(s"$mode mode") {
          val drs1 = DateRanges.from(c.seq1)
          val drs2 = DateRanges.from(c.seq2)
          val result = drs1 `union` drs2
          result.indexedSeq should equal (expectedUnion)
          result should be (normalized)
          (drs2 `union` drs1) should equal (result)
        }

      withMode(c.expectedDayChangeUnion)(DateRange.Mode.DayChange)
      withMode(c.expectedFullDayUnion)(DateRange.Mode.FullDay)
    }
  }

  test(".diff()") {
    case class Case(
      seq1: Seq[DateRange],
      seq2: Seq[DateRange],
      expectedDayChangeDiff: Seq[DateRange],
      expectedFullDayDiff: Seq[DateRange],
    )

    val cases = Table(
      "case",
      Case(
        seq1 =                  Seq(),
        seq2 =                  Seq(),
        expectedDayChangeDiff = Seq(),
        expectedFullDayDiff =   Seq(),
      ),
      Case(
        seq1 =                  Seq( ),
        seq2 =                  Seq(0),
        expectedDayChangeDiff = Seq( ),
        expectedFullDayDiff =   Seq( ),
      ),
      Case(
        seq1 =                  Seq(     ),
        seq2 =                  Seq(0###1),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(     ),
      ),
      Case(
        seq1 =                  Seq(0),
        seq2 =                  Seq( ),
        expectedDayChangeDiff = Seq( ),
        expectedFullDayDiff =   Seq(0),
      ),
      Case(
        seq1 =                  Seq(0    ),
        seq2 =                  Seq(    1),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(0    ),
      ),
      Case(
        seq1 =                  Seq(0),
        seq2 =                  Seq(0),
        expectedDayChangeDiff = Seq( ),
        expectedFullDayDiff =   Seq( ),
      ),
      Case(
        seq1 =                  Seq(    1),
        seq2 =                  Seq(0    ),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(    1),
      ),
      Case(
        seq1 =                  Seq(0        ),
        seq2 =                  Seq(    1###2),
        expectedDayChangeDiff = Seq(         ),
        expectedFullDayDiff =   Seq(0        ),
      ),
      Case(
        seq1 =                  Seq(0    ),
        seq2 =                  Seq(0###1),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(     ),
      ),
      Case(
        seq1 =                  Seq(    1    ),
        seq2 =                  Seq(0#######2),
        expectedDayChangeDiff = Seq(         ),
        expectedFullDayDiff =   Seq(         ),
      ),
      Case(
        seq1 =                  Seq(    1),
        seq2 =                  Seq(0###1),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(     ),
      ),
      Case(
        seq1 =                  Seq(        2),
        seq2 =                  Seq(0###1    ),
        expectedDayChangeDiff = Seq(         ),
        expectedFullDayDiff =   Seq(        2),
      ),
      Case(
        seq1 =                  Seq(0###1),
        seq2 =                  Seq(     ),
        expectedDayChangeDiff = Seq(0###1),
        expectedFullDayDiff =   Seq(0###1),
      ),
      Case(
        seq1 =                  Seq(0###1    ),
        seq2 =                  Seq(        2),
        expectedDayChangeDiff = Seq(0###1    ),
        expectedFullDayDiff =   Seq(0###1    ),
      ),
      Case(
        seq1 =                  Seq(0###1),
        seq2 =                  Seq(    1),
        expectedDayChangeDiff = Seq(0###1),
        expectedFullDayDiff =   Seq(0    ),
      ),
      Case(
        seq1 =                  Seq(0#######2),
        seq2 =                  Seq(    1    ),
        expectedDayChangeDiff = Seq(0#######2),
        expectedFullDayDiff =   Seq(0,      2),
      ),
      Case(
        seq1 =                  Seq(0###1),
        seq2 =                  Seq(0    ),
        expectedDayChangeDiff = Seq(0###1),
        expectedFullDayDiff =   Seq(    1),
      ),
      Case(
        seq1 =                  Seq(    1###2),
        seq2 =                  Seq(0        ),
        expectedDayChangeDiff = Seq(    1###2),
        expectedFullDayDiff =   Seq(    1###2),
      ),
      Case(
        seq1 =                  Seq(0###1        ),
        seq2 =                  Seq(        2###3),
        expectedDayChangeDiff = Seq(0###1        ),
        expectedFullDayDiff =   Seq(0###1        ),
      ),
      Case(
        seq1 =                  Seq(0###1    ),
        seq2 =                  Seq(    1###2),
        expectedDayChangeDiff = Seq(0###1    ),
        expectedFullDayDiff =   Seq(0        ),
      ),
      Case(
        seq1 =                  Seq(0#######2    ),
        seq2 =                  Seq(    1#######3),
        expectedDayChangeDiff = Seq(0###1        ),
        expectedFullDayDiff =   Seq(0            ),
      ),
      Case(
        seq1 =                  Seq(0#######2),
        seq2 =                  Seq(    1###2),
        expectedDayChangeDiff = Seq(0###1    ),
        expectedFullDayDiff =   Seq(0        ),
      ),
      Case(
        seq1 =                  Seq(0###########3),
        seq2 =                  Seq(    1###2    ),
        expectedDayChangeDiff = Seq(0###1,  2###3),
        expectedFullDayDiff =   Seq(0,          3),
      ),
      Case(
        seq1 =                  Seq(0###1    ),
        seq2 =                  Seq(0#######3),
        expectedDayChangeDiff = Seq(         ),
        expectedFullDayDiff =   Seq(         ),
      ),
      Case(
        seq1 =                  Seq(0###1),
        seq2 =                  Seq(0###1),
        expectedDayChangeDiff = Seq(     ),
        expectedFullDayDiff =   Seq(     ),
      ),
      Case(
        seq1 =                  Seq(0#######2),
        seq2 =                  Seq(0###1    ),
        expectedDayChangeDiff = Seq(    1###2),
        expectedFullDayDiff =   Seq(        2),
      ),
      Case(
        seq1 =                  Seq(    1###2    ),
        seq2 =                  Seq(0###########3),
        expectedDayChangeDiff = Seq(             ),
        expectedFullDayDiff =   Seq(             ),
      ),
      Case(
        seq1 =                  Seq(    1###2),
        seq2 =                  Seq(0#######2),
        expectedDayChangeDiff = Seq(         ),
        expectedFullDayDiff =   Seq(         ),
      ),
      Case(
        seq1 =                  Seq(    1#######3),
        seq2 =                  Seq(0#######2    ),
        expectedDayChangeDiff = Seq(        2###3),
        expectedFullDayDiff =   Seq(            3),
      ),
      Case(
        seq1 =                  Seq(    1###2),
        seq2 =                  Seq(0###1    ),
        expectedDayChangeDiff = Seq(    1###2),
        expectedFullDayDiff =   Seq(        2),
      ),
      Case(
        seq1 =                  Seq(        2###3),
        seq2 =                  Seq(0###1        ),
        expectedDayChangeDiff = Seq(        2###3),
        expectedFullDayDiff =   Seq(        2###3),
      ),
      Case(
        seq1 =                  Seq(    1###############5,  6#######8),
        seq2 =                  Seq(0#######2,      4###########7    ),
        expectedDayChangeDiff = Seq(        2#######4,          7###8),
        expectedFullDayDiff =   Seq(            3,                  8),
      ),
      Case(
        seq1 =                  Seq(0###############################8),
        seq2 =                  Seq(    1###2,  3###4,      6###7    ),
        expectedDayChangeDiff = Seq(0###1,  2###3,  4#######6,  7###8),
        expectedFullDayDiff =   Seq(0,                  5,          8),
      ),
    )

    forAll(cases) { c =>
      def withMode(expectedDiff: Seq[DateRange])(implicit mode: DateRange.Mode): Unit =
        withClue(s"$mode mode") {
          val drs1 = DateRanges.from(c.seq1)
          val drs2 = DateRanges.from(c.seq2)
          val result = drs1 `diff` drs2
          result.indexedSeq should equal (expectedDiff)
          result should be (normalized)
        }

      withMode(c.expectedDayChangeDiff)(DateRange.Mode.DayChange)
      withMode(c.expectedFullDayDiff)(DateRange.Mode.FullDay)
    }
  }

  test(".intersect()") {
    case class Case(
      seq1: Seq[DateRange],
      seq2: Seq[DateRange],
      expectedDayChangeIntersect: Seq[DateRange],
      expectedFullDayIntersect: Seq[DateRange],
    )

    val cases = Table(
      "case",
      Case(
        seq1 =                       Seq(),
        seq2 =                       Seq(),
        expectedDayChangeIntersect = Seq(),
        expectedFullDayIntersect =   Seq(),
      ),
      Case(
        seq1 =                       Seq( ),
        seq2 =                       Seq(0),
        expectedDayChangeIntersect = Seq( ),
        expectedFullDayIntersect =   Seq( ),
      ),
      Case(
        seq1 =                       Seq(     ),
        seq2 =                       Seq(0###1),
        expectedDayChangeIntersect = Seq(     ),
        expectedFullDayIntersect =   Seq(     ),
      ),
      Case(
        seq1 =                       Seq(0    ),
        seq2 =                       Seq(    1),
        expectedDayChangeIntersect = Seq(     ),
        expectedFullDayIntersect =   Seq(     ),
      ),
      Case(
        seq1 =                       Seq(0),
        seq2 =                       Seq(0),
        expectedDayChangeIntersect = Seq( ),
        expectedFullDayIntersect =   Seq(0),
      ),
      Case(
        seq1 =                       Seq(0        ),
        seq2 =                       Seq(    1###2),
        expectedDayChangeIntersect = Seq(         ),
        expectedFullDayIntersect =   Seq(         ),
      ),
      Case(
        seq1 =                       Seq(0    ),
        seq2 =                       Seq(0###1),
        expectedDayChangeIntersect = Seq(     ),
        expectedFullDayIntersect =   Seq(0    ),
      ),
      Case(
        seq1 =                       Seq(    1    ),
        seq2 =                       Seq(0#######2),
        expectedDayChangeIntersect = Seq(         ),
        expectedFullDayIntersect =   Seq(    1    ),
      ),
      Case(
        seq1 =                       Seq(    1),
        seq2 =                       Seq(0###1),
        expectedDayChangeIntersect = Seq(     ),
        expectedFullDayIntersect =   Seq(    1),
      ),
      Case(
        seq1 =                       Seq(        2),
        seq2 =                       Seq(0###1    ),
        expectedDayChangeIntersect = Seq(         ),
        expectedFullDayIntersect =   Seq(         ),
      ),
      Case(
        seq1 =                       Seq(0###1        ),
        seq2 =                       Seq(        2###3),
        expectedDayChangeIntersect = Seq(             ),
        expectedFullDayIntersect =   Seq(             ),
      ),
      Case(
        seq1 =                       Seq(0###1    ),
        seq2 =                       Seq(    1###2),
        expectedDayChangeIntersect = Seq(         ),
        expectedFullDayIntersect =   Seq(    1    ),
      ),
      Case(
        seq1 =                       Seq(0#######2    ),
        seq2 =                       Seq(    1#######3),
        expectedDayChangeIntersect = Seq(    1###2    ),
        expectedFullDayIntersect =   Seq(    1###2    ),
      ),
      Case(
        seq1 =                       Seq(0#######2),
        seq2 =                       Seq(    1###2),
        expectedDayChangeIntersect = Seq(    1###2),
        expectedFullDayIntersect =   Seq(    1###2),
      ),
      Case(
        seq1 =                       Seq(0###########3),
        seq2 =                       Seq(    1###2    ),
        expectedDayChangeIntersect = Seq(    1###2    ),
        expectedFullDayIntersect =   Seq(    1###2    ),
      ),
      Case(
        seq1 =                       Seq(0###1    ),
        seq2 =                       Seq(0#######2),
        expectedDayChangeIntersect = Seq(0###1    ),
        expectedFullDayIntersect =   Seq(0###1    ),
      ),
      Case(
        seq1 =                       Seq(0###1),
        seq2 =                       Seq(0###1),
        expectedDayChangeIntersect = Seq(0###1),
        expectedFullDayIntersect =   Seq(0###1),
      ),
      Case(
        seq1 =                       Seq(0###1,      3###############7),
        seq2 =                       Seq(    1###########4,      6###7),
        expectedDayChangeIntersect = Seq(            3###4,      6###7),
        expectedFullDayIntersect =   Seq(    1,      3###4,      6###7),
      ),
    )

    forAll(cases) { c =>
      def withMode(expectedIntersect: Seq[DateRange])(implicit mode: DateRange.Mode): Unit =
        withClue(s"$mode mode") {
          val drs1 = DateRanges.from(c.seq1)
          val drs2 = DateRanges.from(c.seq2)
          val result = drs1 `intersect` drs2
          result.indexedSeq should equal (expectedIntersect)
          result should be (normalized)
          (drs2 `intersect` drs1) should equal (result)
        }

      withMode(c.expectedDayChangeIntersect)(DateRange.Mode.DayChange)
      withMode(c.expectedFullDayIntersect)(DateRange.Mode.FullDay)
    }
  }

  test("set operations with DayChange mode on single-day ranges") {
    val drs1 = DateRanges.single(1, 1)(DateRange.Mode.FullDay)
    val drs2 = DateRanges.single(2, 2)(DateRange.Mode.FullDay)

    import DateRange.Mode.DayChange
    (drs1 `union` drs2).indexedSeq should be (empty)
    (drs1 `diff` DateRanges.empty).indexedSeq should be (empty)
    (drs1 `intersect` drs1).indexedSeq should be (empty)
  }

  test("NormalizedMatcher") {
    import DateRange.Mode.DayChange

    val seq = IndexedSeq[DateRange](2###3, 0###1)

    DateRanges(seq) should not be normalized
    DateRanges.from(seq) should be (normalized)
  }
}

object DateRangesTest {
  private implicit class IntOps(private val int: Int) extends AnyVal {
    def ###(otherInt: Int): DateRange = DateRange(int, otherInt)
    def #######(otherInt: Int): DateRange = int ### otherInt
    def ###########(otherInt: Int): DateRange = int ### otherInt
    def ###############(otherInt: Int): DateRange = int ### otherInt
    def ###################(otherInt: Int): DateRange = int ### otherInt
    def #######################(otherInt: Int): DateRange = int ### otherInt
    def ###########################(otherInt: Int): DateRange = int ### otherInt
    def ###############################(otherInt: Int): DateRange = int ### otherInt
  }

  private class NormalizedMatcher(implicit mode: DateRange.Mode) extends BeMatcher[DateRanges] {
    override def apply(seq: DateRanges): MatchResult =
      MatchResult(
        seq == DateRanges.from(seq.indexedSeq),
        s"$seq is not normalized",
        s"$seq is normalized",
      )
  }

  private def normalized(implicit mode: DateRange.Mode) = new NormalizedMatcher()
}
