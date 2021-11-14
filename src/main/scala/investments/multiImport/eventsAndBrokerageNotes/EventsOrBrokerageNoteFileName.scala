package sisgrana
package investments.multiImport.eventsAndBrokerageNotes

import investments.files.FileName
import investments.files.filters.{FilterFunction, HasFilesFilter}
import java.time.LocalDate
import scala.util.matching.Regex

sealed trait EventsOrBrokerageNoteFileName extends FileName {
  def date: LocalDate
}

case class EventsFileName(date: LocalDate) extends EventsOrBrokerageNoteFileName
case class BrokerageNoteFileName(date: LocalDate, stockbroker: String) extends EventsOrBrokerageNoteFileName

object EventsOrBrokerageNoteFileName extends HasFilesFilter[EventsOrBrokerageNoteFileName] {
  private val Regex: Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r

  override def FilterFunction: FilterFunction[EventsOrBrokerageNoteFileName] = {
    case Regex(dateString, label) =>
      val date = LocalDate.parse(dateString)
      if (label == "EVENTS") {
        EventsFileName(date)
      } else {
        BrokerageNoteFileName(date, stockbroker = label)
      }
  }
}
