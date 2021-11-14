package sisgrana
package investments.fileTypes.quotes

import java.io.InputStream
import scala.io.Source

object QuotesFileReader {
  def readFrom(inputStream: InputStream): Iterator[Quotes] =
    for {
      line <- Source.fromInputStream(inputStream).getLines()
      if line.startsWith("01")
    } yield new Quotes(line)
}
