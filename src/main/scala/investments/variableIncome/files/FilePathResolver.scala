package sisgrana
package investments.variableIncome.files

import java.io.{File, FileInputStream, InputStream}
import java.util.zip.ZipInputStream
import scala.annotation.tailrec
import utils.MapFoldLeft._
import utils._

object FilePathResolver {
  class NonExistingPathException(val path: String, cause: Throwable = null) extends Exception(cause)

  def resolve(paths: Seq[String]): Seq[MultiLevelFilePath] =
    paths.flatMap(resolve).distinct

  def resolve(path: String): Seq[MultiLevelFilePath] =
    FileSystemContext.resolve(path)

  private object FileSystemContext {
    def resolve(path: String): Seq[MultiLevelFilePath] = {
      val (existingPath, suffixOpt) = splitExistingPathPrefix(path)
      try {
        resolve(existingPath, suffixOpt)
      } catch {
        case e: NonExistingPathException => throw new NonExistingPathException(path, e)
      }
    }

    private def resolve(existingPath: File, suffixOpt: Option[String]): Seq[MultiLevelFilePath] = {
      if (existingPath.isDirectory) {
        suffixOpt match {
          case Some(suffix) => throw new NonExistingPathException(suffix)
          case None =>
            for {
              child <- existingPath.list().toSeq
              path <- resolve(new File(existingPath, child), None)
            } yield path
        }
      } else if (existingPath.getName.toLowerCase.endsWith(".zip")) {
        def processZipFile(f: ZipFileContext => Seq[MultiLevelFilePath]): Seq[InsideZipFilePath] =
          ZipFileContext.process(existingPath.getPath, new FileInputStream(existingPath), f)
        suffixOpt match {
          case Some(suffix) => processZipFile(_.resolve(suffix))
          case None => processZipFile(_.all())
        }
      } else {
        suffixOpt match {
          case Some(suffix) => throw new NonExistingPathException(suffix)
          case None => Seq(TerminalFilePath(existingPath.getPath))
        }
      }
    }

    private def splitExistingPathPrefix(path: String): (File, Option[String]) = {
      @tailrec
      def loop(path: String, suffixParts: Seq[String]): (File, Option[String]) = {
        val file = new File(path)
        if (file.exists()) {
          val suffix = Option.when(suffixParts.nonEmpty) {
            suffixParts.mkString("/")
          }
          (file, suffix)
        } else {
          val i = path.lastIndexOf('/')
          if (i >= 0) {
            val newPath = path.take(i)
            val part = path.drop(i + 1)
            loop(newPath, part +: suffixParts)
          } else {
            throw new NonExistingPathException(path)
          }
        }
      }

      loop(path, Seq.empty)
    }
  }

  private class ZipFileContext(inputStream: InputStream) {
    private val zipInputStream = new ZipInputStream(inputStream)
    private val entries = Iterator.continually(zipInputStream.getNextEntry).takeWhile(_ != null)

    def resolve(path: String): Seq[MultiLevelFilePath] = {
      val (directoryConfirmed, filePathSeqs) = entries.mapFoldLeft(false) { (directoryConfirmed, entry) =>
        def returnDefault = (Seq.empty, directoryConfirmed)
        def returnFilePaths(filePaths: Seq[MultiLevelFilePath]) = (filePaths, directoryConfirmed)
        def returnDirectoryConfirmed = (Seq.empty, true)

        if (entry.isDirectory) {
          if (entry.getName.stripTrailingSlash == path) {
            returnDirectoryConfirmed
          } else {
            returnDefault
          }
        } else {
          if (entry.getName == path  ||  entry.getName.startsWith(path.withTrailingSlash)) {
            if (entry.getName.toLowerCase.endsWith(".zip")) {
              returnFilePaths(ZipFileContext.process(entry.getName, zipInputStream, _.all()))
            } else {
              returnFilePaths(Seq(TerminalFilePath(entry.getName)))
            }
          } else if (path.startsWith(entry.getName.withTrailingSlash)) {
            val suffix = path.drop(entry.getName.withTrailingSlash.length)
            returnFilePaths(ZipFileContext.process(entry.getName, zipInputStream, _.resolve(suffix)))
          } else {
            returnDefault
          }
        }
      }

      val filePaths = filePathSeqs.flatten.toSeq
      if (filePaths.isEmpty  &&  !directoryConfirmed) {
        throw new NonExistingPathException(path)
      }

      filePaths
    }

    def all(): Seq[MultiLevelFilePath] =
      (
        for {
          entry <- entries
          if !entry.isDirectory
          filePath <-
            if (entry.getName.toLowerCase.endsWith(".zip")) {
              ZipFileContext.process(entry.getName, zipInputStream, _.all())
            } else {
              Seq(TerminalFilePath(entry.getName))
            }
        } yield filePath
      ).toSeq
  }

  private object ZipFileContext {
    def process(zipFilePath: String, inputStream: InputStream, f: ZipFileContext => Seq[MultiLevelFilePath]): Seq[InsideZipFilePath] = {
      val context = new ZipFileContext(inputStream)
      for (filePath <- f(context))
        yield InsideZipFilePath(zipFilePath, filePath)
    }
  }

  private implicit class StringOps(private val string: String) extends AnyVal {
    def stripTrailingSlash: String =
      string.pipeIf(string.endsWith("/"))(_.dropRight(1))

    def withTrailingSlash: String =
      string.pipeIf(!string.endsWith("/"))(_ ++ "/")
  }
}
