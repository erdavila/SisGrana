package sisgrana
package investments.files.filters

import investments.files.FileName

trait HasFilesFilter[A <: FileName] {
  def FilterFunction: FilterFunction[A]
  final def FilesFilter: Filter[A] = Filter(FilterFunction)
}
