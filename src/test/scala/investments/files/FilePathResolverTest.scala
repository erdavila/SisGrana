package sisgrana
package investments.files

import investments.files.FilePathResolver.NonExistingPathException
import java.io.File

class FilePathResolverTest extends TestBase {
  import FilePathResolverTest._

  private val BasePath = getClass.getClassLoader.getResource(".").getPath / FilePathResolver.getClass.getSimpleName

  new File(BasePath / "empty-dir").mkdir()

  test(".resolve()") {
    val cases = Table(
      "path" -> "expected file paths",

      BasePath / "text.txt" -> Seq(
        TerminalFilePath(BasePath / "text.txt"),
      ),
      BasePath / "empty-dir" -> Seq.empty,
      BasePath / "zip.zip/at-zip-root.txt" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("at-zip-root.txt")),
      ),
      BasePath / "zip.zip/empty-dir" -> Seq.empty,
      BasePath / "zip.zip/dir/in-zip-dir.txt" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("dir/in-zip-dir.txt")),
      ),
      BasePath / "zip.zip/dir/zip-inside-zip.zip/in-zip-inside-zip.txt" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", InsideZipFilePath("dir/zip-inside-zip.zip", TerminalFilePath("in-zip-inside-zip.txt")))
      ),
      BasePath / "zip.zip/dir/zip-inside-zip.zip" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", InsideZipFilePath("dir/zip-inside-zip.zip", TerminalFilePath("in-zip-inside-zip.txt")))
      ),
      BasePath / "zip.zip/dir" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("dir/in-zip-dir.txt")),
        InsideZipFilePath(BasePath / "zip.zip", InsideZipFilePath("dir/zip-inside-zip.zip", TerminalFilePath("in-zip-inside-zip.txt")))
      ),
      BasePath / "zip.zip" -> Seq(
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("at-zip-root.txt")),
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("dir/in-zip-dir.txt")),
        InsideZipFilePath(BasePath / "zip.zip", InsideZipFilePath("dir/zip-inside-zip.zip", TerminalFilePath("in-zip-inside-zip.txt")))
      ),
      BasePath -> Seq(
        TerminalFilePath(BasePath / "text.txt"),
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("at-zip-root.txt")),
        InsideZipFilePath(BasePath / "zip.zip", TerminalFilePath("dir/in-zip-dir.txt")),
        InsideZipFilePath(BasePath / "zip.zip", InsideZipFilePath("dir/zip-inside-zip.zip", TerminalFilePath("in-zip-inside-zip.txt")))
      ),
    )

    forAll(cases) { (path, expectedFilePaths) =>
      val result = FilePathResolver.resolve(path)

      result should contain theSameElementsAs expectedFilePaths
    }
  }

  test(".resolve() non-existing path") {
    val cases = Table(
      "non-existing path",
      "non-existing-file.txt",
      BasePath / "non-existing-file.txt",
      BasePath / "non-existing-dir/non-existing-file.txt",
      BasePath / "non-existing-dir/",
      BasePath / "non-existing-zip.zip",
      BasePath / "text.txt/file.txt",
      BasePath / "zip.zip/non-existing-file.txt",
      BasePath / "zip.zip/non-existing-dir/",
      BasePath / "zip.zip/non-existing-zip.zip",
      BasePath / "zip.zip/at-zip-root.txt/file.txt",
      BasePath / "zip.zip/dir/non-existing-file.txt",
      BasePath / "zip.zip/dir/zip-inside-zip.zip/non-existing-file.txt",
      BasePath / "zip.zip/dir/zip-inside-zip.zip/non-existing-dir/",
    )

    forAll(cases) { nonExistingPath =>
      val e = the [NonExistingPathException] thrownBy FilePathResolver.resolve(nonExistingPath)

      e.path should endWith (nonExistingPath)
    }
  }
}

object FilePathResolverTest {
  private implicit class PathStringOps(private val path: String) extends AnyVal {
    def / (child: String): String = new File(path, child).getPath
  }
}
