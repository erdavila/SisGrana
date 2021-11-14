package sisgrana
package investments.multiImport.nonQuoteDates

import investments.files.FileName
import investments.files.filters.{FilterFunction, HasFilesFilter}

sealed trait NonQuoteDatesFileName extends FileName

case object NonQuoteDatesFileName extends NonQuoteDatesFileName with HasFilesFilter[NonQuoteDatesFileName] {
  val FileName = "NON-QUOTE DATES.ssv"

  override def FilterFunction: FilterFunction[NonQuoteDatesFileName] = {
    case FileName => NonQuoteDatesFileName
  }
}
