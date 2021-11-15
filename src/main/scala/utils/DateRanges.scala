package sisgrana
package utils

import java.time.LocalDate
import scala.annotation.tailrec
import utils.DateRange.Mode
import utils.DateRanges.removeSingleDayRangesOnDayChangeMode

case class DateRanges private(indexedSeq: IndexedSeq[DateRange]) {
  import utils.dateOrdering._

  def isEmpty: Boolean = indexedSeq.isEmpty

  def nonEmpty: Boolean = !isEmpty

  def contains(date: LocalDate): Boolean = {
    val result = PredicateBinarySearch.search(indexedSeq) { range =>
      if (date < range.beginDate) PredicateBinarySearch.ContinueSearchingBefore
      else if (date > range.endDate) PredicateBinarySearch.ContinueSearchingAfter
      else PredicateBinarySearch.StopSearching
    }
    result.found
  }

  def union(other: DateRanges)(implicit mode: DateRange.Mode): DateRanges =
    doSetOperation(other, DateRanges.union)

  def diff(other: DateRanges)(implicit mode: DateRange.Mode): DateRanges =
    doSetOperation(other, DateRanges.diff)

  def intersect(other: DateRanges)(implicit mode: DateRange.Mode): DateRanges =
    doSetOperation(other, DateRanges.intersect)

  private def doSetOperation(other: DateRanges, op: (IndexedSeq[DateRange], IndexedSeq[DateRange]) => IndexedSeq[DateRange])(implicit mode: DateRange.Mode): DateRanges = {
    val newISeq = op(this.indexedSeq, other.indexedSeq)
    DateRanges(removeSingleDayRangesOnDayChangeMode(newISeq))
  }
}

object DateRanges {
  import utils.dateOrdering._

  lazy val empty: DateRanges = DateRanges(IndexedSeq.empty)

  private[utils] def apply(iSeq: IndexedSeq[DateRange]): DateRanges =
    new DateRanges(iSeq)

  def single(beginDate: LocalDate, endDate: LocalDate)(implicit mode: DateRange.Mode): DateRanges =
    single(DateRange(beginDate, endDate))

  def single(dateRange: DateRange)(implicit mode: DateRange.Mode): DateRanges =
    from(Seq(dateRange))

  def from(seq: Seq[DateRange])(implicit mode: DateRange.Mode): DateRanges =
    DateRanges(normalize(seq))

  private def normalize(seq: Seq[DateRange])(implicit mode: DateRange.Mode): IndexedSeq[DateRange] = {
    @tailrec
    def loop(queue: IndexedSeq[IndexedSeq[DateRange]]): IndexedSeq[DateRange] =
      queue match {
        case drs0 +: drs1 +: rest => loop(rest :+ union(drs0, drs1))
        case drs0 +: _ => drs0
        case _ => IndexedSeq.empty
      }

    val queue =
      for (dr <- removeSingleDayRangesOnDayChangeMode(seq.toIndexedSeq))
        yield IndexedSeq(dr)

    loop(queue)
  }

  private def union(iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange])(implicit mode: DateRange.Mode): IndexedSeq[DateRange] = {
    @tailrec
    def loop(dr: Option[DateRange], iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange], result: Vector[DateRange]): IndexedSeq[DateRange] =
      (dr, iSeq1, iSeq2) match {
        case (Some(dr), dr1 +: rest1, _) if dr.endDate.plusDays(mode.offset) >= dr1.beginDate =>
          val newDr = dr.copy(endDate = max(dr.endDate, dr1.endDate))
          loop(Some(newDr), rest1, iSeq2, result)
        case (Some(dr), _, dr2 +: rest2) if dr.endDate.plusDays(mode.offset) >= dr2.beginDate =>
          val newDr = dr.copy(endDate = max(dr.endDate, dr2.endDate))
          loop(Some(newDr), iSeq1, rest2, result)
        case (Some(dr), _,  _) => loop(None, iSeq1, iSeq2, result :+ dr)
        case (None, dr1 +: rest1, dr2 +: _) if dr1.beginDate < dr2.beginDate => loop(Some(dr1), rest1, iSeq2, result)
        case (None, _ +: _, dr2 +: rest2) => loop(Some(dr2), iSeq1, rest2, result)
        case (None, Nil, _) => result ++ iSeq2
        case _ => result ++ iSeq1
      }

    loop(None, iSeq1, iSeq2, Vector.empty)
  }

  private def diff(iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange])(implicit mode: DateRange.Mode): IndexedSeq[DateRange] = {
    @tailrec
    def loop(iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange], result: Vector[DateRange]): IndexedSeq[DateRange] =
      (iSeq1, iSeq2) match {
        case (dr1 +: rest1, dr2 +: rest2) =>
          if (dr1.beginDate < dr2.beginDate) {
            if (dr1.endDate < dr2.beginDate) {
              // dr1: |--|
              // dr2:      |--...
              loop(rest1, iSeq2, result :+ dr1)
            } else if(dr1.endDate > dr2.beginDate) {
              // dr1: |-----|
              // dr2:    |--...
              val newDr = dr1.copy(endDate = dr2.beginDate.minusDays(mode.offset))
              loop(dr1.copy(beginDate = dr2.beginDate) +: rest1, iSeq2, result :+ newDr)
            } else {
              // dr1: |--|
              // dr2:    |--...
              val newDr = dr1.copy(endDate = dr2.beginDate.minusDays(mode.offset))
              loop(rest1, iSeq2, result :+ newDr)
            }
          }else if (dr1.beginDate > dr2.beginDate) {
            if (dr2.endDate < dr1.beginDate) {
              // dr1:      |--|
              // dr2: |--|
              loop(iSeq1, rest2, result)
            } else {
              // dr1:    |--|
              // dr2: |---...
              loop(iSeq1, dr2.copy(beginDate = dr1.beginDate) +: rest2, result)
            }
          } else {
            if (dr1.endDate < dr2.endDate) {
              // dr1: |--|
              // dr2: |-----|
              loop(rest1, iSeq2, result)
            } else if (dr1.endDate > dr2.endDate) {
              // dr1: |-----|
              // dr2: |--|
              loop(dr1.copy(beginDate = dr2.endDate.plusDays(mode.offset)) +: rest1, rest2, result)
            } else {
              // dr1: |--|
              // dr2: |--|
              loop(rest1, rest2, result)
            }
          }
        case (dr1 +: rest1, Nil) => (result :+ dr1) ++ rest1
        case _ => result
      }

    loop(iSeq1, iSeq2, Vector.empty)
  }

  private def intersect(iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange])(implicit mode: DateRange.Mode): IndexedSeq[DateRange] = {
    @tailrec
    def loop(iSeq1: IndexedSeq[DateRange], iSeq2: IndexedSeq[DateRange], result: Vector[DateRange]): IndexedSeq[DateRange] =
      (iSeq1, iSeq2) match {
        case (dr1 +: rest1, dr2 +: rest2)=>
          val intersectionBeginDate = max(dr1.beginDate, dr2.beginDate)
          val intersectionEndDate = min(dr1.endDate, dr2.endDate)
          val intersectionDrOpt = Option.when(intersectionBeginDate < intersectionEndDate.plusDays(mode.offset)) {
            DateRange(intersectionBeginDate, intersectionEndDate)
          }

          if (dr1.endDate < dr2.endDate) {
            loop(rest1, iSeq2, result ++ intersectionDrOpt)
          } else if (dr1.endDate > dr2.endDate) {
            loop(iSeq1, rest2, result ++ intersectionDrOpt)
          } else {
            loop(rest1, rest2, result ++ intersectionDrOpt)
          }
        case _ => result
      }

    loop(iSeq1, iSeq2, Vector.empty)
  }

  private def removeSingleDayRangesOnDayChangeMode(iSeq: IndexedSeq[DateRange])(implicit mode: DateRange.Mode): IndexedSeq[DateRange] =
    iSeq.pipeIf(mode == Mode.DayChange)(_.filter(dr => dr.beginDate != dr.endDate))
}
