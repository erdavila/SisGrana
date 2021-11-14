package sisgrana
package investments

import investments.model.StockbrokerAsset
import utils.DateRanges

package object incomeRate {
  type Portfolio = Map[StockbrokerAsset, DateRanges]
}
