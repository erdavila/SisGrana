package sisgrana
package investments.model

import java.time.LocalDate

case class AssetQuote(
  asset: String,
  date: LocalDate,
  openPrice: Double,
  closePrice: Double,
  minPrice: Double,
  avgPrice: Double,
  maxPrice: Double,
)
