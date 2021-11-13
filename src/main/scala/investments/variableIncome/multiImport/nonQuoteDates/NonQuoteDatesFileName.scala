package sisgrana
package investments.variableIncome.multiImport.nonQuoteDates

import investments.variableIncome.files.FileName
import investments.variableIncome.files.filters.{FilterFunction, HasFilesFilter}

sealed trait NonQuoteDatesFileName extends FileName

object NonQuoteDatesFileName extends NonQuoteDatesFileName with HasFilesFilter[NonQuoteDatesFileName] {
  val FileName = "NON-QUOTE DATES.ssv"

  override def FilterFunction: FilterFunction[NonQuoteDatesFileName] = {
    case FileName => NonQuoteDatesFileName
  }
}
