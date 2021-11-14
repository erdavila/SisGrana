package sisgrana
package investments.commands.multiImport.eventsAndBrokerageNotes

import investments.AssetType
import investments.fileTypes.names.NamesFileReader
import scala.annotation.tailrec
import utils.quoted

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
  def get(): NameNormalizer = {
    val entries = NamesFileReader.read()
    new NameNormalizer(entries.toMap)
  }
}
