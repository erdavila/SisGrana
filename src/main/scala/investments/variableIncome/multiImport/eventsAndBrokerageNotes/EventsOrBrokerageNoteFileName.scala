package sisgrana
package investments.variableIncome.multiImport.eventsAndBrokerageNotes

import investments.variableIncome.files.FileName
import investments.variableIncome.files.filters.Filter
import java.time.LocalDate
import scala.util.matching.Regex

sealed trait EventsOrBrokerageNoteFileName extends FileName {
  def date: LocalDate
}

case class EventsFileName(date: LocalDate) extends EventsOrBrokerageNoteFileName
case class BrokerageNoteFileName(date: LocalDate, stockbroker: String) extends EventsOrBrokerageNoteFileName

object EventsOrBrokerageNoteFileName {
  private val Regex: Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r

  val FilesFilter: Filter[EventsOrBrokerageNoteFileName] = Filter {
    case Regex(dateString, label) =>
      val date = LocalDate.parse(dateString)
      if (label == "EVENTS") {
        EventsFileName(date)
      } else {
        BrokerageNoteFileName(date, stockbroker = label)
      }
  }
}
