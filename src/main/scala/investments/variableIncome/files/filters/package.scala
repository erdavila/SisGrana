package sisgrana
package investments.variableIncome.files

import cats.data.State
import investments.variableIncome.files.FileName.QuotesFileName
import java.time.{LocalDate, Year => JYear, YearMonth => JYearMonth}

package object filters {
  type Paths = Seq[MultiLevelFilePath]
  type Filter[A <: FileName] = State[Paths, Seq[InputFile[A]]]

  object Filter {
    def apply[A <: FileName](f: PartialFunction[String, A]): Filter[A] =
      State { paths =>
        paths.partitionMap { path =>
          f.lift(path.name) match {
            case Some(fileName) => Right(InputFile(fileName, path))
            case None => Left(path)
          }
        }
      }

    def apply[A](filter: State[Paths, A])(filePaths: Seq[MultiLevelFilePath]): A = {
      val (remainingPaths, a) = filter.run(filePaths).value
      if (remainingPaths.nonEmpty) {
        throw new IllegalArgumentException("Arquivos inválidos: " ++ remainingPaths.map(_.stringPath).mkString(", "))
      }
      a
    }
  }


  private val YearQuotesRegex = """COTAHIST_A(\d{4})\.TXT""".r
  private val MonthQuotesRegex = """COTAHIST_M(\d{2})(\d{4})\.TXT""".r
  private val DateQuotesRegex = """COTAHIST_D(\d{2})(\d{2})(\d{4}).TXT""".r

  val QuotesFiles: Filter[QuotesFileName] = Filter {
    case YearQuotesRegex(yearString) =>
      val year = JYear.parse(yearString)
      QuotesFileName(QuotesFileName.Year(year))
    case MonthQuotesRegex(monthString, yearString) =>
      val yearMonth = JYearMonth.parse(s"$yearString-$monthString")
      QuotesFileName(QuotesFileName.Month(yearMonth))
    case DateQuotesRegex(dayString, monthString, yearString) =>
      val date = LocalDate.parse(s"$yearString-$monthString-$dayString")
      QuotesFileName(QuotesFileName.Date(date))
  }
}
