package sisgrana
package investments.variableIncome.model

import investments.variableIncome.model.ctx._

trait Persisted extends LocalDateSupport {
  def persisted(assetPeriodSeqs: Seq[AssetPeriod]*): Any = {
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
}
