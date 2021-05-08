package sisgrana
package investments.irpf

import java.io.File
import java.time.LocalDate
import scala.util.matching.Regex

sealed trait FileName {
  def date: LocalDate
}

object FileName {
  object select {
    private val FileNameRegex: Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r
    def unapply(file: File): Option[FileName] =
      file.getName match {
        case FileNameRegex(dateString, name) =>
          val date = LocalDate.parse(dateString)
          val values =
            if (name == "EVENTS") EventsFileName(date, file)
            else BrokerageNoteFileName(date, name, file)
          Some(values)
        case _ => None
      }
  }
}

case class EventsFileName(override val date: LocalDate, file: File)
  extends FileName

case class BrokerageNoteFileName(override val date: LocalDate, stockbroker: String, file: File)
  extends FileName
