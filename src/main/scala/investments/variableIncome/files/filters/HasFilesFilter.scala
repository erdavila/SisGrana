package sisgrana
package investments.variableIncome.files.filters

import investments.variableIncome.files.FileName

trait HasFilesFilter[A <: FileName] {
  def FilterFunction: FilterFunction[A]
  final def FilesFilter: Filter[A] = Filter(FilterFunction)
}
