package sisgrana
package investments.variableIncome.importAssets

import java.io.File
import java.time.LocalDate
import scala.util.matching.Regex

case class ImportedFileName(date: LocalDate, label: String, file: File)

object ImportedFileName {
  private val Regex: Regex = """^(\d{4}-\d{2}-\d{2}) - (.+)\.ssv$""".r

  def from(file: File): Option[ImportedFileName] =
    file.getName match {
      case Regex(dateString, label) =>
        val date = LocalDate.parse(dateString)
        Some(ImportedFileName(date, label, file))
      case _ => None
    }
}
