package sisgrana
package investments.variableIncome

import investments.variableIncome.model.StockbrokerAsset
import utils.DateRanges

package object incomeRate {
  type Portfolio = Map[StockbrokerAsset, DateRanges]
}
