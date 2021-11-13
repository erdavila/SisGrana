package sisgrana
package investments.variableIncome.multiImport.nonQuoteDates

import investments.variableIncome.files.FileName
import investments.variableIncome.files.filters.Filter

object NonQuoteDatesFileName extends FileName {
  val FileName = "NON-QUOTE DATES.ssv"

  val FilesFilter: Filter[NonQuoteDatesFileName.type] = Filter {
    case FileName => NonQuoteDatesFileName
  }
}
