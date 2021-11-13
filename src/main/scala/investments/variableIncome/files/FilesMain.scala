package sisgrana
package investments.variableIncome.files

import investments.variableIncome.AssetType
import investments.variableIncome.multiImport.eventsAndBrokerageNotes.{BrokerageNoteFileName, EventsFileName, EventsOrBrokerageNoteFileName, NameNormalizer}
import investments.variableIncome.multiImport.nonQuoteDates.NonQuoteDatesFileName
import investments.variableIncome.quotesFiles.QuotesFileName

object FilesMain {
  def main(args: Array[String]): Unit =
    ArgsParser.parse(args) match {
      case OperationArguments.Resolve(path) => resolve(path)
    }

  private def resolve(path: String): Unit =
    for(path <- FilePathResolver.resolve(path).sortBy(_.stringPath)) {
      println(s"${formattedPath(path)}   ${formattedType(path)}")
    }

  private def formattedPath(path: MultiLevelFilePath): String =
    path match {
      case TerminalFilePath(filePath) => filePath
      case InsideZipFilePath(zipFilePath, inZipMultiLevelFilePath) =>
        val levelSeparator = Console.RED ++ Console.BOLD ++ "/" ++ Console.RESET
        zipFilePath ++ levelSeparator ++ formattedPath(inZipMultiLevelFilePath)
    }

  private case object DatabaseFileName extends FileName
  private case object NameNormalizerFileName extends FileName
  private case object TypesFileName extends FileName

  private val AllTypesFilterFunction: String => Option[FileName] =
    EventsOrBrokerageNoteFileName.FilterFunction
      .orElse(QuotesFileName.FilterFunction)
      .orElse(NonQuoteDatesFileName.FilterFunction)
      .orElse[String, FileName] {
        case s"$_.sqlite" => DatabaseFileName
        case NameNormalizer.FileName => NameNormalizerFileName
        case AssetType.Resolver.TypesFileName => TypesFileName
      }
      .lift

  private def formattedType(path: MultiLevelFilePath): String = {
    val (color, text) = AllTypesFilterFunction(path.name) match {
      case Some(EventsFileName(date)) => (Console.BLUE, s"Eventos de $date")
      case Some(BrokerageNoteFileName(date, stockbroker)) => (Console.GREEN, s"Nota de corretagem de $date, corretora $stockbroker")
      case Some(NonQuoteDatesFileName) => (Console.MAGENTA, "Datas sem cotação")
      case Some(QuotesFileName(period)) =>
        val text = period match {
          case QuotesFileName.Year(year) => s"Cotações do ano $year"
          case QuotesFileName.Month(month) => s"Cotações do mês $month"
          case QuotesFileName.Date(date) => s"Cotações da data $date"
        }
        (Console.YELLOW, text)
      case Some(DatabaseFileName) => (Console.CYAN, "Base de dados")
      case Some(NameNormalizerFileName) => (Console.CYAN, "Nomes para normalização")
      case Some(TypesFileName) => (Console.CYAN, "Tipos de ativos")
      case Some(fileName) => (Console.RED, s"TIPO IDENTIFICADO MAS NÃO ESPERADO: $fileName")
      case None => (Console.RED, "Tipo desconhecido")
    }
    color ++ "[" ++ text ++ "]" ++ Console.RESET
  }
}
