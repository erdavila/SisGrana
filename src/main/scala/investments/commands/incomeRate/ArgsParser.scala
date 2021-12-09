package sisgrana
package investments.commands.incomeRate

import investments.ArgumentsParser
import java.io.PrintStream
import utils.quoted

object ArgsParser extends ArgumentsParser[(Boolean, Period, Seq[AssetFilter], Seq[AssetFilter])] {
  val ResolvePortfolioOption = "--resolve-portfolio"

  override protected def spec: Parser[(Boolean, Period, Seq[AssetFilter], Seq[AssetFilter])] =
    for {
      resolvePortfolio <- takeOption(ResolvePortfolioOption)
      period <- takePeriod
      qualifiedFilters <- takeQualifiedFilters
      (positiveFilters, negativeFilters) = separateByQualification(qualifiedFilters)
      _ = if (positiveFilters.isEmpty) error("Ao menos um filtro positivo é obrigatório")
    } yield (resolvePortfolio, period, positiveFilters, negativeFilters)

  private def takePeriod: Parser[Period] =
    for (str <- takeNext)
      yield Period.parse(str)

  private def takeQualifiedFilters: Parser[List[(AssetFilter, Boolean)]] =
    takeRemaining $$ toQualifiedFilter

  private def toQualifiedFilter(arg: String): (AssetFilter, Boolean) =
    arg match {
      case s"-$str" =>
        val filter = toFilter(str)
        if (filter == AssetFilter()) {
          error(s"Filtro negativo inválido: ${quoted(arg)}")
        }
        (filter, false)
      case str => (toFilter(str), true)
    }

  private def toFilter(arg: String): AssetFilter =
    AssetFilter.parse(arg)

  private def separateByQualification(qualifiedFilters: Seq[(AssetFilter, Boolean)]): (Seq[AssetFilter], Seq[AssetFilter]) =
    qualifiedFilters
      .partitionMap {
        case (filter, true) => Left(filter)
        case (filter, false) => Right(filter)
      }

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println(s"Parâmetros esperados: [$ResolvePortfolioOption] PERÍODO FILTRO...")
    printStream.println()
    printStream.println("    PERÍODO pode ser:")
    printStream.println("        ANO")
    printStream.println("        ANO-MÊS")
    printStream.println("        ANO-INICIAL:ANO-FINAL")
    printStream.println("        ANO-MÊS-INICIAL:ANO-MÊS-FINAL")
    printStream.println("        DATA-INICIAL:DATA-FINAL")
    printStream.println()
    printStream.println("    FILTRO tem o formato:")
    printStream.println("        [-][ATIVO][:CORRETORA][@CARTEIRA][>DATA-INICIAL][<DATA-FINAL]")
  }
}
