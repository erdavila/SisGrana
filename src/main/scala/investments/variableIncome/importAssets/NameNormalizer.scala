package sisgrana
package investments.variableIncome.importAssets

import investments.variableIncome.{AssetType, DataPath}
import java.io.File
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
