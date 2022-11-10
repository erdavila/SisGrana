package sisgrana
package utils

object BrWord {
  private type FlexedWordByCount = Int => BrWord.Flexed

  val dia: FlexedWordByCount = sSuffixed("dia")
  val identificado: FlexedWordByCount = sSuffixed("identificado")
  val mãe: FlexedWordByCount = sSuffixed("mãe")
  val mão: FlexedWordByCount = ãosSuffixed("m")
  val obtido: FlexedWordByCount = sSuffixed("obtido")
  val pão: FlexedWordByCount = ãesSuffixed("p")
  val preço: FlexedWordByCount = sSuffixed("preço")
  val ser: FlexedWordByCount = emSuffixed("ser")

  private def sSuffixed(singularWord: String): FlexedWordByCount = suffixed(singularWord, "s")
  private def emSuffixed(singularWord: String): FlexedWordByCount = suffixed(singularWord, "em")
  private def ãesSuffixed(radical: String): FlexedWordByCount = specificSuffixed(radical, "ão", "ães")
  private def ãosSuffixed(radical: String): FlexedWordByCount = specificSuffixed(radical, "ão", "ãos")

  private def suffixed(singularWord: String, suffix: String): FlexedWordByCount =
    (count: Int) => {
      val flexedWord = if (count == 1) singularWord else singularWord ++ suffix
      new BrWord.Flexed(flexedWord, count)
    }

  private def specificSuffixed(radical: String, singularSuffix: String, pluralSuffix: String): FlexedWordByCount =
    (count: Int) => {
      val suffix = if (count == 1) singularSuffix else pluralSuffix
      val flexedWord = radical ++ suffix
      new BrWord.Flexed(flexedWord, count)
    }

  class Flexed(flexedWord: String, count: Int) {
    override def toString: String = flexedWord
    def withCount: String = s"$count $flexedWord"
  }
}
