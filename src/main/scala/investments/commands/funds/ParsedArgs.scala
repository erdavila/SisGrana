package sisgrana
package investments.commands.funds

import java.time.YearMonth

case class ParsedArgs(
  month: YearMonth,
  printOptions: Printer.Options,
)
