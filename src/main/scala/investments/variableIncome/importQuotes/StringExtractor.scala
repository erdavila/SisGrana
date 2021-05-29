package sisgrana
package investments.variableIncome.importQuotes

class StringExtractor[A](initialPosition: Int, finalPosition: Int, transform: String => A) {
  def from(string: String): A = {
    val substring = string.substring(initialPosition - 1, finalPosition)
    transform(substring)
  }
}

object StringExtractor {
  class PriceExtractor(initialPosition: Int)
    extends StringExtractor(initialPosition, initialPosition + 12, str => {
      str.toDouble / 100.0
    })
}
