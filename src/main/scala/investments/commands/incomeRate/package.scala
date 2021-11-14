package sisgrana
package investments.commands

import investments.model.StockbrokerAsset
import utils.DateRanges

package object incomeRate {
  type Portfolio = Map[StockbrokerAsset, DateRanges]
}
