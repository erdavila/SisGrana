package sisgrana
package investments.files.filters

import cats.data.State
import investments.files.{FileName, InputFile}

object Filter {
  def apply[A <: FileName](f: FilterFunction[A]): Filter[A] =
    State { paths =>
      paths.partitionMap { path =>
        f.lift(path.name) match {
          case Some(fileName) => Right(InputFile(fileName, path))
          case None => Left(path)
        }
      }
    }
}
