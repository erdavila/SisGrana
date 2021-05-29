package sisgrana
package investments.variableIncome

import io.getquill.{SnakeCase, SqliteJdbcContext}

package object model {
  lazy val ctx = new SqliteJdbcContext(SnakeCase, "ctx")
}
