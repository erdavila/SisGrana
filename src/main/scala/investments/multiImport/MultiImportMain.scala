package sisgrana
package investments.multiImport

import investments.fileTypes.EventsOrBrokerageNoteFileName
import investments.fileTypes.nonQuoteDates.NonQuoteDatesFileName
import investments.fileTypes.quotes.QuotesFileName
import investments.files.filters.applyFilter

object MultiImportMain {
  private val FilesFilter =
    for {
      eventsAndBrokerageNotes <- EventsOrBrokerageNoteFileName.FilesFilter
      quotes <- QuotesFileName.FilesFilter
      nonQuoteDates <- NonQuoteDatesFileName.FilesFilter
    } yield (eventsAndBrokerageNotes, quotes, nonQuoteDates)

  def main(args: Array[String]): Unit = {
    val (options, filePaths) = ArgsParser.parse(args)
    val (eventsAndBrokerageNotesInputFiles, quotesInputFiles, nonQuoteDatesInputFiles) = applyFilter(FilesFilter)(filePaths)
    eventsAndBrokerageNotes.Processor.process(eventsAndBrokerageNotesInputFiles, options.filterAssetsFromDate, options.resetAssets)
    quotes.Processor.process(quotesInputFiles)
    nonQuoteDates.Processor.process(nonQuoteDatesInputFiles)
  }
}
