package sisgrana
package investments.commands.funds.operations.getPrices

object UpdatePriceTag {
  private val value = "[UPDATE-PRICE]"

  def isIn(str: String): Boolean = str.startsWith(value)

  def removeFrom(str: String): String =
    if (str.startsWith(value)) str.drop(value.length)
    else str
}
