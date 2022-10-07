package sisgrana
package investments.commands.funds

import java.time.YearMonth

case class ParsedArgs(
  initialMonth: YearMonth,
  finalMonth: YearMonth,
  printOptions: ChunkMaker.Options,
  positiveFilters: Seq[String],
  negativeFilters: Seq[String],
)
