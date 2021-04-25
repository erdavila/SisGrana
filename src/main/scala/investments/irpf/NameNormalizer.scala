package sisgrana
package investments.irpf

import java.io.File
import scala.annotation.tailrec

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
  def fromFile(file: File): NameNormalizer = {
    val entries = for {
      names <- SSV.readFile(file)
      normalizedName = names.head
      name <- names
    } yield name -> normalizedName
    new NameNormalizer(entries.toMap)
  }
}
