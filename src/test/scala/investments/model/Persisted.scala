package sisgrana
package investments.model

import investments.model.ctx._

trait Persisted extends LocalDateSupport {
  def persisted(assetPeriodSeqs: Seq[AssetPeriod]*): Unit = {
    val assetPeriods = assetPeriodSeqs.flatten
    ctx.transaction {
      ctx.run(query[AssetPeriod].delete)
      ctx.run(
        for (ap <- liftQuery(assetPeriods)) {
          query[AssetPeriod].insert(ap)
        }
      )
    }
  }

  def persisted(portfolios: Portfolio*)(implicit dummy: DummyImplicit): Unit = {
    ctx.transaction {
      ctx.run(query[PortfolioAssetDateRange].delete)
      for (p <- portfolios) {
        Portfolio.save(p)
      }
    }
  }
}
