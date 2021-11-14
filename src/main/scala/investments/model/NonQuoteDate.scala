package sisgrana
package investments.model

import investments.model.ctx._
import java.time.{DayOfWeek, LocalDate}
import scala.annotation.tailrec

case class NonQuoteDate(date: LocalDate)

object NonQuoteDate extends LocalDateSupport {
  @tailrec
  def adjustToQuoteDate(date: LocalDate): LocalDate =
    date.getDayOfWeek match {
      case DayOfWeek.SUNDAY => adjustToQuoteDate(date.minusDays(2))
      case DayOfWeek.SATURDAY => adjustToQuoteDate(date.minusDays(1))
      case _ if isNonQuoteDate(date) => adjustToQuoteDate(date.minusDays(1))
      case _ => date
    }

  private def isNonQuoteDate(date: LocalDate): Boolean =
    ctx.run(
      query[NonQuoteDate].filter(_.date == lift(date)).nonEmpty
    )
}
