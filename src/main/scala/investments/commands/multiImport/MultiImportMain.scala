package sisgrana
package investments.commands.multiImport

import investments.fileTypes.EventsOrBrokerageNoteFileName
import investments.fileTypes.fundsMonthStatement.FundsMonthStatementFileName
import investments.fileTypes.nonQuoteDates.NonQuoteDatesFileName
import investments.fileTypes.quotes.QuotesFileName
import investments.files.filters.applyFilter

object MultiImportMain {
  private val FilesFilter =
    for {
      eventsAndBrokerageNotes <- EventsOrBrokerageNoteFileName.FilesFilter
      quotes <- QuotesFileName.FilesFilter
      nonQuoteDates <- NonQuoteDatesFileName.FilesFilter
      fundsMonthStatement <- FundsMonthStatementFileName.FilesFilter
    } yield (eventsAndBrokerageNotes, quotes, nonQuoteDates, fundsMonthStatement)

  def main(args: Array[String]): Unit = {
    val (options, filePaths) = ArgsParser.parse(args)
    val (eventsAndBrokerageNotesInputFiles, quotesInputFiles, nonQuoteDatesInputFiles, fundsMonthStatement) = applyFilter(FilesFilter)(filePaths)
    if (fundsMonthStatement.nonEmpty) {
      println("Ignorando os arquivos:")
      for (inputFile <- fundsMonthStatement) {
        println(s"  ${inputFile.path.stringPath}")
      }
    }

    eventsAndBrokerageNotes.Processor.process(eventsAndBrokerageNotesInputFiles, options.filterAssetsFromDate, options.resetAssets)
    quotes.Processor.process(quotesInputFiles)
    nonQuoteDates.Processor.process(nonQuoteDatesInputFiles)
  }
}
