package sisgrana
package investments.model

import java.time.LocalDate
import utils.DateRange

case class PortfolioAssetDateRange(portfolioName: String, asset: String, stockbroker: String, beginDate: LocalDate, endDate: LocalDate) {
  def stockbrokerAsset: StockbrokerAsset = StockbrokerAsset(stockbroker, asset)
  def dateRange: DateRange = DateRange(beginDate, endDate)
}

object PortfolioAssetDateRange {
  def apply(portfolioName: String, stockbrokerAsset: StockbrokerAsset, dateRange: DateRange): PortfolioAssetDateRange =
    PortfolioAssetDateRange(portfolioName, stockbrokerAsset.asset, stockbrokerAsset.stockbroker, dateRange.beginDate, dateRange.endDate)
}
