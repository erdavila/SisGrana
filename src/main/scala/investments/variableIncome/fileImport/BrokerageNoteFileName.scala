package sisgrana
package investments.variableIncome.fileImport

import java.io.File
import java.time.LocalDate

case class BrokerageNoteFileName(date: LocalDate, stockbroker: String, file: File)

object BrokerageNoteFileName {
  val Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r
}
