package sisgrana
package investments.variableIncome.importAssets

import java.io.File
import java.time.LocalDate
import scala.util.matching.Regex

sealed trait ImportedFileName {
  def date: LocalDate
  def file: File
}

case class BrokerageNoteFileName(date: LocalDate, stockbroker: String, file: File)
  extends ImportedFileName

object BrokerageNoteFileName {
  val Regex: Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r
}

case class EventsFileName(date: LocalDate, file: File)
  extends ImportedFileName

object EventsFileName {
  val Regex: Regex = """^(\d{4}-\d{2}-\d{2}) - EVENTS\.ssv$""".r
}
