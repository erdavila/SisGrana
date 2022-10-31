package sisgrana
package investments.commands.funds

import java.time.YearMonth

case class MonthRange(initialMonth: YearMonth, finalMonth: YearMonth) {
  def iterator: Iterator[YearMonth] =
    Iterator.iterate(initialMonth)(_.plusMonths(1))
      .takeWhile(month => !month.isAfter(finalMonth))
}
