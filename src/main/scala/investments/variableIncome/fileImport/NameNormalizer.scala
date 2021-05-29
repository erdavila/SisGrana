package sisgrana
package investments.variableIncome.fileImport

import java.io.File
import scala.annotation.tailrec
import utils.quoted

class NameNormalizer(map: Map[String, String]) {
  def normalize(name: String): String = {
    @tailrec
    def loop(name: String): Option[String] =
      map.get(name) match {
        case o@Some(_) => o
        case None if name.lengthIs > 1 => loop(name.dropRight(1).trim)
        case None => None
      }

    loop(name) match {
      case Some(normalized) => normalized
      case None =>
        Console.err.println(s"Unknown name: ${quoted(name)}")
        name
    }
  }
}

object NameNormalizer {
  private val FileName = "names.ssv"

  def get(): NameNormalizer = {
    val entries = for {
      names <- SSV.readFile(new File(DataPath, FileName))
      normalizedName = names.head
      name <- names
    } yield name -> normalizedName
    new NameNormalizer(entries.toMap)
  }
}
