package sisgrana
package investments.irpf

import java.io.File

class Names(normalizationMap: Map[String, String]) {
  def normalize(name: String): String = {
    normalizationMap.get(name) match {
      case Some(normalized) => normalized
      case None =>
        Console.err.println(s"Unknown name: ${quoted(name)}")
        name
    }
  }
}

object Names {
  def fromFile(file: File): Names = {
    val map = TSV.fromFile(file) { lines =>
      (
        for {
          elements <- lines
          code = elements.head
          altName <- elements
        } yield altName -> code
      ).toMap
    }

    new Names(map)
  }
}
