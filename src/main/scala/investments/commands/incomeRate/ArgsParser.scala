package sisgrana
package investments.commands.incomeRate

import investments.ArgumentsParser
import java.io.PrintStream
import utils.quoted

object ArgsParser extends ArgumentsParser[(Boolean, PeriodsSpec, Seq[AssetFilter], Seq[AssetFilter])] {
  val ResolvePortfolioOption = "--resolve-portfolio"

  override protected def spec: Parser[(Boolean, PeriodsSpec, Seq[AssetFilter], Seq[AssetFilter])] =
    for {
      resolvePortfolio <- takeOption(ResolvePortfolioOption)
      periodSpec <- takePeriodSpec
      qualifiedFilters <- takeQualifiedFilters
      (positiveFilters, negativeFilters) = separateByQualification(qualifiedFilters)
      _ = if (positiveFilters.isEmpty) error("Ao menos um filtro positivo é obrigatório")
    } yield (resolvePortfolio, periodSpec, positiveFilters, negativeFilters)

  private def takePeriodSpec: Parser[PeriodsSpec] =
    for (str <- takeNext)
      yield PeriodsSpec.parse(str)

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
    printStream.println(s"Parâmetros esperados: [$ResolvePortfolioOption] PERÍODOS FILTRO...")
    printStream.println()
    printStream.println("    PERÍODOS é")
    printStream.println("        PERÍODO-INICIAL[/[DESLOCAMENTO:]NÚMERO-DE-PERÍODOS]")
    printStream.println()
    printStream.println("        PERÍODO-INICIAL pode ser:")
    printStream.println("            ANO")
    printStream.println("            ANO-MÊS")
    printStream.println("            ANO-INICIAL:ANO-FINAL")
    printStream.println("            ANO-MÊS-INICIAL:ANO-MÊS-FINAL")
    printStream.println("            DATA-INICIAL:DATA-FINAL")
    printStream.println()
    printStream.println("        DESLOCAMENTO")
    printStream.println("            Define o quanto cada período avança em relação ao período anterior, em unidades do PERÍODO-INICIAL.")
    printStream.println("            Por padrão, é igual ao tamanho do PERÍODO-INICIAL.")
    printStream.println()
    printStream.println("        NÚMERO-DE-PERÍODOS")
    printStream.println("            Por padrão, é 1.")
    printStream.println()
    printStream.println("        Exemplos:")
    printStream.println("            1) \"2021-01/6\" (equivalente a \"2021-01/1:6\") define os períodos:")
    printStream.println("                2021-01")
    printStream.println("                2021-02")
    printStream.println("                2021-03")
    printStream.println("                2021-04")
    printStream.println("                2021-05")
    printStream.println("                2021-06")
    printStream.println("            2) \"2021-01:2021-03/4\" (equivalente a \"2021-01:2021-03/3:4\") define os períodos:")
    printStream.println("                2021-01:2021-03")
    printStream.println("                2021-04:2021-06")
    printStream.println("                2021-07:2021-09")
    printStream.println("                2021-10:2021-12")
    printStream.println("            2) \"2021-01:2021-03/2:5\" define os períodos:")
    printStream.println("                2021-01:2021-03")
    printStream.println("                2021-03:2021-05")
    printStream.println("                2021-05:2021-07")
    printStream.println("                2021-07:2021-09")
    printStream.println("                2021-09:2021-11")
    printStream.println()
    printStream.println("    FILTRO tem o formato:")
    printStream.println("        [-][ATIVO][:CORRETORA][@CARTEIRA][>DATA-INICIAL][<DATA-FINAL]")
  }
}
