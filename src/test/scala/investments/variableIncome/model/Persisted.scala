package sisgrana
package investments.variableIncome.model

import investments.variableIncome.model.ctx._

trait Persisted extends LocalDateSupport {
  def persisted(assetChangeSeqs: Seq[AssetChange]*): Any = {
    val assetChanges = assetChangeSeqs.flatten
    ctx.transaction {
      ctx.run(query[AssetChange].delete)
      ctx.run(
        for (ac <- liftQuery(assetChanges)) {
          query[AssetChange].insert(ac)
        }
      )
    }
  }
}
