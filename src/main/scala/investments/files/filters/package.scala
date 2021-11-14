package sisgrana
package investments.files

import cats.data.State

package object filters {
  type Paths = Seq[MultiLevelFilePath]
  type Filter[A <: FileName] = State[Paths, Seq[InputFile[A]]]
  type FilterFunction[A <: FileName] = PartialFunction[String, A]

  def applyFilter[A](filter: State[Paths, A])(filePaths: Seq[MultiLevelFilePath]): A = {
    val (remainingPaths, a) = filter.run(filePaths).value
    if (remainingPaths.nonEmpty) {
      throw new IllegalArgumentException("Arquivos inválidos: " ++ remainingPaths.map(_.stringPath).mkString(", "))
    }
    a
  }
}
