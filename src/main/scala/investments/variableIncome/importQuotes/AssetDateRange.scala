package sisgrana
package investments.variableIncome.importQuotes

import investments.variableIncome.model.AssetChange
import java.time.LocalDate
import scala.annotation.tailrec

case class AssetDateRange(beginDate: LocalDate, endDate: LocalDate) {
  private val ord = implicitly[Ordering[LocalDate]]
  import ord._

  require(beginDate <= endDate)

  def contains(date: LocalDate): Boolean =
    date >= beginDate && date <= endDate
}

object AssetDateRange {
  private val ord = implicitly[Ordering[LocalDate]]
  import ord._

  def seqFromAssetChanges(sortedAssetChanges: Seq[AssetChange], minDate: LocalDate, maxDate: LocalDate): Seq[AssetDateRange] = {

    object AssetChangeZero {
      def unapply(ac: AssetChange): Option[LocalDate] =
        Option.when(ac.resultingPositionQuantity == 0)(ac.date)
    }

    object AssetChangeNonZero {
      def unapply(ac: AssetChange): Option[LocalDate] =
        Option.when(ac.resultingPositionQuantity != 0)(ac.date)
    }

    @tailrec
    def loop(result: Seq[AssetDateRange], beginDateOpt: Option[LocalDate], seq: Seq[AssetChange]): Seq[AssetDateRange] =
      (beginDateOpt, seq) match {
        case (Some(beginDate), AssetChangeZero(endDate) +: t) => loop(result :+ AssetDateRange(beginDate, endDate), None, t)
        case (Some(_), AssetChangeNonZero(_) +: t) => loop(result, beginDateOpt, t)
        case (Some(beginDate), _) => result :+ AssetDateRange(beginDate, maxDate)
        case (None, AssetChangeZero(_) +: t) => loop(result, None, t)
        case (None, AssetChangeNonZero(beginDate) +: t) => loop(result, Some(max(beginDate, minDate)), t)
        case (None, _) => result
      }

    loop(Vector.empty, None, sortedAssetChanges)
  }

  def mergeSeqs(seq1: Seq[AssetDateRange], seq2: Seq[AssetDateRange]): Seq[AssetDateRange] = {
    @tailrec
    def loop(result: Seq[AssetDateRange], currentRangeOpt: Option[AssetDateRange], seq1: Seq[AssetDateRange], seq2: Seq[AssetDateRange]): Seq[AssetDateRange] = {
      def extractEarliestRange(seq1: Seq[AssetDateRange], seq2: Seq[AssetDateRange]): (AssetDateRange, Seq[AssetDateRange], Seq[AssetDateRange]) =
        if (seq1.head.beginDate < seq2.head.beginDate) {
          (seq1.head, seq1.tail, seq2)
        } else {
          (seq2.head, seq1, seq2.tail)
        }

      def tryToExtendRange(current: AssetDateRange, extension: AssetDateRange): Option[AssetDateRange] =
        Option.when(current.contains(extension.beginDate)) {
          AssetDateRange(current.beginDate, max(current.endDate, extension.endDate))
        }

      (currentRangeOpt, seq1, seq2) match {
        case (Some(currRange), _ +: _, _ +: _) =>
          val (candidateRange, newSeq1, newSeq2) = extractEarliestRange(seq1, seq2)
          tryToExtendRange(currRange, candidateRange) match {
            case newCurrRangeOpt@Some(_) => loop(result, newCurrRangeOpt, newSeq1, newSeq2)
            case None => loop(result :+ currRange, Some(candidateRange), newSeq1, newSeq2)
          }
        case (None, _ +: _, _ +: _) =>
          val (earliestRange, newSeq1, newSeq2) = extractEarliestRange(seq1, seq2)
          loop(result, Some(earliestRange), newSeq1, newSeq2)
        case (Some(currRange), h1 +: t1, Nil) =>
          tryToExtendRange(currRange, h1) match {
            case newCurrRangeOpt@Some(_) => loop(result, newCurrRangeOpt, t1, Nil)
            case None => loop(result :+ currRange, Some(h1), t1, Nil)
          }
        case (None, h1 +: t1, Nil) => loop(result, Some(h1), t1, Nil)
        case (_, Nil, _ +: _) => loop(result, currentRangeOpt, seq2, Nil)
        case (_, Nil, Nil) => result ++ currentRangeOpt
        case _ => throw new Exception("Should never happen!")
      }
    }

    loop(Vector.empty, None, seq1, seq2)
  }
}
