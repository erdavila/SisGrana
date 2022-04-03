package sisgrana
package investments

sealed trait Taxation

object Taxation {
  case object ExemptableSwingTrade extends Taxation
  case object NonExemptableStocksSwingTrade extends Taxation
  case object NonExemptableOptionsSwingTrade extends Taxation
  case object FII extends Taxation
  case object NonVariableIncome extends Taxation
}
