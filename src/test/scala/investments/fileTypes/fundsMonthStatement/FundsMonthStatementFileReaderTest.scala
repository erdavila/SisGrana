package sisgrana
package investments.fileTypes.fundsMonthStatement

import java.io.ByteArrayInputStream
import java.time.{Month, YearMonth}

class FundsMonthStatementFileReaderTest extends TestBase {
  test(".readFrom()") {
    val yearMonth = YearMonth.of(2022, Month.JANUARY)
    val reader = new FundsMonthStatementFileReader(yearMonth)
    val inputString =
      """
        |INI  A   15,14315266   18,70299972
        |INI  B   85,56099707  138,16051767
        |
        |12  A  15,24343893
        |12  B  87,45314298  +57,99800678
        |12  C  18,90379667   -0,07778442  "Resgate total"
        |
        |20  no-prices
        |
        |""".stripMargin
    val bytes = inputString.getBytes("UTF-8")
    val inputStream = new ByteArrayInputStream(bytes)

    val statement = reader.readFrom(inputStream)

    statement.initialEntries.keySet should equal (Set("A", "B"))
    statement.initialEntries("A") should equal (FundsStatement.InitialEntry(15.14315266, BigDecimal( 18.70299972), None))
    statement.initialEntries("B") should equal (FundsStatement.InitialEntry(85.56099707, BigDecimal(138.16051767), None))

    statement.entries.keySet should equal (Set(yearMonth.atDay(12)))
    statement.entries(yearMonth.atDay(12)).keySet should equal (Set("A", "B", "C"))
    statement.entries(yearMonth.atDay(12))("A") should equal (FundsStatement.Entry(15.24343893, None, None))
    statement.entries(yearMonth.atDay(12))("B") should equal (FundsStatement.Entry(87.45314298, Some(BigDecimal(57.99800678)), None))
    statement.entries(yearMonth.atDay(12))("C") should equal (FundsStatement.Entry(18.90379667, Some(BigDecimal(-0.07778442)), Some("Resgate total")))

    statement.noPriceDates should equal (Set(yearMonth.atDay(20)))
  }
}
