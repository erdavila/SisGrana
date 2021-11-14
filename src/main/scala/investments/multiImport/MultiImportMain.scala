package sisgrana
package investments.multiImport

import investments.files.filters.applyFilter
import investments.multiImport.eventsAndBrokerageNotes.EventsOrBrokerageNoteFileName
import investments.multiImport.nonQuoteDates.NonQuoteDatesFileName
import investments.quotesFiles.QuotesFileName

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
