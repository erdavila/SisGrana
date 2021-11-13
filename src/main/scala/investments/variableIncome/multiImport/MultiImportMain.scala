package sisgrana
package investments.variableIncome.multiImport

import investments.variableIncome.files.filters
import investments.variableIncome.files.filters.Filter
import investments.variableIncome.multiImport.eventsAndBrokerageNotes.EventsOrBrokerageNoteFileName
import investments.variableIncome.multiImport.nonQuoteDates.NonQuoteDatesFileName

object MultiImportMain {
  private val FilesFilter =
    for {
      eventsAndBrokerageNotes <- EventsOrBrokerageNoteFileName.FilesFilter
      quotes <- filters.QuotesFiles
      nonQuoteDates <- NonQuoteDatesFileName.FilesFilter
    } yield (eventsAndBrokerageNotes, quotes, nonQuoteDates)

  def main(args: Array[String]): Unit = {
    val (options, filePaths) = ArgsParser.parse(args)
    val (eventsAndBrokerageNotesInputFiles, quotesInputFiles, nonQuoteDatesInputFiles) = Filter.apply(FilesFilter)(filePaths)
    eventsAndBrokerageNotes.Processor.process(eventsAndBrokerageNotesInputFiles, options.filterAssetsFromDate, options.resetAssets)
    quotes.Processor.process(quotesInputFiles)
    nonQuoteDates.Processor.process(nonQuoteDatesInputFiles)
  }
}
