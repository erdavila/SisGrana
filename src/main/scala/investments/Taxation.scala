package sisgrana
package investments

sealed trait Taxation

object Taxation {
  case object DayTrade
  case object ExemptableSwingTrade extends Taxation
  case object NonExemptableSwingTrade extends Taxation
  case object FII extends Taxation
  case object NonVariableIncome extends Taxation
}
