package sisgrana
package investments.irpf

import java.io.{File, PrintWriter}
import scala.io.Source

object TSV {
  type Elements = List[String]

  object Elements {
    def apply(values: Any*): Elements =
      values.map(_.toString).toList

    def matching[A](elements: Elements)(pf: PartialFunction[List[String], A]): A =
      pf.apply(elements)
  }

  def fromFile[A](file: File)(f: Iterator[Elements] => A) : A =
    try {
      use(Source.fromFile(file)) { source =>
        val content = source.getLines().map { str =>
          if (str.isEmpty) Nil
          else str.split('\t').toList
        }
        f(content)
      }
    } catch {
      case e: Throwable => throw new Exception(s"Exception while processing file ${file.getName}", e)
    }

  def writeFile(file: File)(content: Iterable[Elements]): Unit =
    use(new PrintWriter(file)) { writer =>
      for (elements <- content) {
        writer.println(elements.mkString("\t"))
      }
    }
}
