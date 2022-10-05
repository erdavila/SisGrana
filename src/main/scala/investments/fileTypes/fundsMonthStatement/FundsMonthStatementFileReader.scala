package sisgrana
package investments.fileTypes.fundsMonthStatement

import com.softwaremill.quicklens._
import investments.files.SSV
import java.io.{FileInputStream, InputStream}
import java.time.YearMonth
import utils.BrNumber

private[fundsMonthStatement] class FundsMonthStatementFileReader(yearMonth: YearMonth) {
  private object PositiveIntToString {
    def unapply(str: String): Option[Int] = str.toIntOption.filter(_ > 0)
  }

  def readFrom(inputStream: InputStream): FundsStatement = {
    val emptyStatement = FundsStatement(Map.empty, Map.empty, Set.empty)
    SSV.readFrom(inputStream).foldLeft(emptyStatement) { (statement, lineValues) =>
      SSV.matchValues(lineValues) {
        case Seq(PositiveIntToString(day), fund, sharePrice) => addEntry(statement, day, fund, sharePrice, None, None)
        case Seq(PositiveIntToString(day), fund, sharePrice, shareAmountChange) => addEntry(statement, day, fund, sharePrice, Some(shareAmountChange), None)
        case Seq(PositiveIntToString(day), fund, sharePrice, shareAmountChange, note) => addEntry(statement, day, fund, sharePrice, Some(shareAmountChange), Some(note))
        case Seq(iniString, fund, sharePrice, shareAmount) if isIni(iniString) => addInitialEntry(statement, fund, sharePrice, shareAmount, None)
        case Seq(iniString, fund, sharePrice, shareAmount, note) if isIni(iniString) => addInitialEntry(statement, fund, sharePrice, shareAmount, Some(note))
        case Seq(PositiveIntToString(day), "no-prices") => setNoPricesDay(statement, day)
        case Seq() => statement
      }
    }
  }

  private def isIni(string: String) = string.equalsIgnoreCase("INI")

  private def addEntry(
    statement: FundsStatement,
    day: Int,
    fund: String,
    sharePriceString: String,
    shareAmountChangeString: Option[String],
    note: Option[String],
  ): FundsStatement = {
    val date = yearMonth.atDay(day)
    if (statement.noPriceDates.contains(date)) {
      throwConflictException(day)
    } else {
      statement.modify(_.entries.atOrElse(date, Map.empty)).using { dateEntries =>
        if (dateEntries.contains(fund)) {
          throw new Exception(s"Múltiplos registros no dia $day para o fundo $fund")
        } else {
          val sharePrice = BrNumber.parse(sharePriceString)
          val shareAmountChange = shareAmountChangeString.map(BrNumber.parseBigDecimal)
          val newEntry = FundsStatement.Entry(sharePrice, shareAmountChange, note)
          dateEntries + (fund -> newEntry)
        }
      }
    }
  }

  private def addInitialEntry(
    statement: FundsStatement,
    fund: String,
    sharePriceString: String,
    shareAmountString: String,
    note: Option[String],
  ): FundsStatement =
    if (statement.initialEntries.contains(fund)) {
      throw new Exception(s"Múltiplos registros iniciais para o fundo $fund")
    } else {
      val sharePrice = BrNumber.parse(sharePriceString)
      val shareAmount = BrNumber.parseBigDecimal(shareAmountString)
      val newInitialEntry = FundsStatement.InitialEntry(sharePrice, shareAmount, note)
      statement.modify(_.initialEntries).using(_ + (fund -> newInitialEntry))
    }

  private def setNoPricesDay(statement: FundsStatement, day: Int): FundsStatement = {
    val date = yearMonth.atDay(day)
    if (statement.entries.contains(date)) {
      throwConflictException(day)
    } else {
      statement.modify(_.noPriceDates).using(_ + date)
    }
  }

  private def throwConflictException(day: Int) =
    throw new Exception(s"Dia $day está marcado como não tendo preços, mas tem registro de preço")
}

object FundsMonthStatementFileReader {
  def read(yearMonth: YearMonth): FundsStatement = {
    val fis = new FileInputStream(s"data/${yearMonth.getYear}/$yearMonth - FUNDS.ssv")
    val reader = new FundsMonthStatementFileReader(yearMonth)
    reader.readFrom(fis)
  }
}