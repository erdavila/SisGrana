package sisgrana
package investments.commands.multiImport

import investments.ArgumentsParser
import investments.files.MultiLevelFilePath
import java.io.PrintStream
import java.time.LocalDate

case class Options(filterAssetsFromDate: Option[LocalDate], resetAssets: Boolean)

object ArgsParser extends ArgumentsParser[(Options, Seq[MultiLevelFilePath])] with ArgumentsParser.Utils {
  override protected def spec: ArgsParser.Parser[(Options, Seq[MultiLevelFilePath])] =
    for {
      filterDate <- takeOptionParameter("--filter-assets-from", "--filter") $ (_.map(toDate))
      reset <- takeOption("--reset-assets", "--reset")
      filePaths <- takeRemaining(1) $ toPaths
    } yield (Options(filterDate, reset), filePaths)

  override protected def printUsage(printStream: PrintStream): Unit = {
    printStream.println("Parâmetros esperados: [OPÇÃO...] CAMINHO...")
    printStream.println()
    printStream.println("    OPÇÃO pode ser:")
    printStream.println()
    printStream.println("        --filter-assets-from DATA")
    printStream.println("        --filter DATA")
    printStream.println("            Considera somente arquivos de notas de corretagem e de eventos a partir de DATA.")
    printStream.println()
    printStream.println("        --reset-assets")
    printStream.println("        --reset")
    printStream.println("            Apaga dados já existentes de ativos a partir da data mais antiga de nota de corretagem ou evento importados.")
    printStream.println()
    printStream.println("    CAMINHO pode ser:")
    printStream.println("        arquivo de nota de corretagem")
    printStream.println("        arquivo de eventos")
    printStream.println("        diretório (todos os arquivos dentro serão considerados)")
    printStream.println("        arquivo .zip (todos os arquivos dentro serão considerados)")
  }
}
