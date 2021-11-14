package sisgrana
package investments.multiImport.eventsAndBrokerageNotes

import investments.DataPath
import investments.AssetType
import java.io.{File, FileInputStream}
import scala.annotation.tailrec
import utils.{SSV, quoted, use}

class NameNormalizer(map: Map[String, String]) {
  def normalize(name: String): String =
    if (AssetType.Resolver.isOption(name)) {
      name
    } else {
      @tailrec
      def loop(name: String): Option[String] =
        map.get(name) match {
          case o@Some(_) => o
          case None if name.lengthIs > 1 => loop(name.dropRight(1).trim)
          case None => None
        }

      loop(name) match {
        case Some(normalized) => normalized
        case None => throw new Exception(s"Ativo desconhecido: ${quoted(name)}")
      }
    }
}

object NameNormalizer {
  val FileName = "names.ssv"

  def get(): NameNormalizer = {
    val entries = use(new FileInputStream(new File(DataPath, FileName))) { inputStream =>
      val entries = for {
        names <- SSV.readFrom(inputStream)
        normalizedName = names.head
        name <- names
      } yield name -> normalizedName
      entries.toMap
    }
    new NameNormalizer(entries.toMap)
  }
}
