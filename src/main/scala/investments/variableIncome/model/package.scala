package sisgrana
package investments.variableIncome

import io.getquill.{SnakeCase, SqliteJdbcContext}
import scala.io.Source

package object model {
  lazy val ctx: SqliteJdbcContext[SnakeCase.type] = {
    val ctx = new SqliteJdbcContext(SnakeCase, "ctx")
    ensureSchema(ctx)
    ctx
  }

  private def ensureSchema(ctx: SqliteJdbcContext[SnakeCase.type]): Unit = {
    import ctx._
    case class SqliteSchema(name: String)
    val schemaSize = ctx.run(query[SqliteSchema].size)
    if (schemaSize == 0) {
      val schemaSource = Source.fromFile("sql/schema.sql")
      try {
        for {
          part <- schemaSource.mkString.split(';')
          sql = part.trim
          if sql.nonEmpty
        } {
          ctx.executeAction(sql ++ ";")
        }
      } finally {
        schemaSource.close()
      }
    }
  }
}
