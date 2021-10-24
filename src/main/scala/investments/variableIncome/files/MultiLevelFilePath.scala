package sisgrana
package investments.variableIncome.files

import java.io.{File, FileInputStream, InputStream}
import java.util.zip.ZipInputStream
import utils.use

sealed trait MultiLevelFilePath {
  def name: String
  def stringPath: String
  def read[A](f: InputStream => A): A
}

case class TerminalFilePath(filePath: String) extends MultiLevelFilePath {
  override def name: String = new File(filePath).getName
  override def stringPath: String = filePath

  override def read[A](f: InputStream => A): A =
    use(new FileInputStream(filePath))(f)
}

case class InsideZipFilePath(zipFilePath: String, inZipMultiLevelFilePath: MultiLevelFilePath) extends MultiLevelFilePath {
  override def name: String = inZipMultiLevelFilePath.name
  override def stringPath: String = s"$zipFilePath/${inZipMultiLevelFilePath.stringPath}"

  override def read[A](f: InputStream => A): A =
    use(new ZipInputStream(new FileInputStream(zipFilePath))) {
      readInsideZip(_, inZipMultiLevelFilePath, f)
    }

  private def readInsideZip[A](zipInputStream: ZipInputStream, path: MultiLevelFilePath, f: InputStream => A): A = {
    def skipToEntry(child: String): Unit = {
      var entry = zipInputStream.getNextEntry
      while (entry.getName != child) {
        entry = zipInputStream.getNextEntry
      }
    }

    path match {
      case TerminalFilePath(filePath) =>
        skipToEntry(filePath)
        f(zipInputStream)
      case InsideZipFilePath(zipFilePath, inZipMultiLevelFilePath) =>
        skipToEntry(zipFilePath)
        use(new ZipInputStream(zipInputStream))(
          readInsideZip(_, inZipMultiLevelFilePath, f)
        )
    }
  }
}
