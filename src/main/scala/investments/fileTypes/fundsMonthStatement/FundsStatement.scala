package sisgrana
package investments.fileTypes.fundsMonthStatement

import java.time.LocalDate

case class FundsStatement(
  initialEntries: Map[String, FundsStatement.InitialEntry],
  entries: Map[LocalDate, Map[String, FundsStatement.Entry]],
  noPriceDates: Set[LocalDate],
)

object FundsStatement {
  case class InitialEntry(
    sharePrice: Double,
    shareAmount: BigDecimal,
    note: Option[String],
  )

  case class Entry(
    sharePrice: Double,
    shareAmountChange: Option[BigDecimal],
    note: Option[String],
  ) {
    require(!shareAmountChange.contains(BigDecimal(0)))
  }
}
