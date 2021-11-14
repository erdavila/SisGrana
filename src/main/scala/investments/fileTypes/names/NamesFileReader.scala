package sisgrana
package investments.fileTypes.names

import investments.files.{DataPath, SSV}
import java.io.{File, FileInputStream, InputStream}
import utils.use

object NamesFileReader {
  val FileName = "names.ssv"

  def read(): Seq[(String, String)] =
    use(new FileInputStream(new File(DataPath, FileName))) { inputStream =>
      readFrom(inputStream).toSeq
    }

  def readFrom(inputStream: InputStream): Iterator[(String, String)] =
    for {
      names <- SSV.readFrom(inputStream)
      if names.nonEmpty
      normalizedName = names.head
      name <- names
    } yield name -> normalizedName
}
