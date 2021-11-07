package sisgrana
package investments.variableIncome.multiImport

import investments.variableIncome.files.filters.Filter
import investments.variableIncome.multiImport.eventsAndBrokerageNotes.EventsOrBrokerageNoteFileName

object MultiImportMain {
  def main(args: Array[String]): Unit = {
    val (options, filePaths) = ArgsParser.parse(args)
    val eventsAndBrokerageNotesInputFiles = Filter.apply(EventsOrBrokerageNoteFileName.FilesFilter)(filePaths)
    eventsAndBrokerageNotes.Processor.process(eventsAndBrokerageNotesInputFiles, options.filterAssetsFromDate, options.resetAssets)
  }
}
