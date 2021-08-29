package sisgrana
package utils

import java.time.LocalDate
import java.time.temporal.ChronoUnit

case class DateRange(beginDate: LocalDate, endDate: LocalDate) {
  import utils.dateOrdering._
  require(beginDate <= endDate)

  def size(implicit mode: DateRange.Mode): Long =
    ChronoUnit.DAYS.between(beginDate, endDate) + mode.offset
}

object DateRange {
  sealed abstract class Mode(val offset: Int)

  object Mode {
    implicit case object DayChange extends Mode(0)
    implicit case object FullDay extends Mode(1)
  }
}
