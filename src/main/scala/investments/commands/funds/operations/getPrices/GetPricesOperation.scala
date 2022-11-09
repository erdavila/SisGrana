package sisgrana
package investments.commands.funds.operations.getPrices

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.softwaremill.quicklens._
import investments.commands.funds.{Missing, MonthStatementChunkMaker, OperationArguments, RecordSet, StatementProcessor}
import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import utils.AnsiString.{Code, StringOps}
import utils.{AnsiString, BrWord, Exit, HttpClient, TextAligner}

object GetPricesOperation {
  private val SuffixTimestampFormat = DateTimeFormatter.ofPattern("YYYY-MM-dd_HH-mm-ss")

  private implicit val system: ActorSystem[_] = ActorSystem(Behaviors.empty, "HttpClient")
  private val carteiraGlobal = new CarteiraGlobal(new HttpClient)

  def execute(args: OperationArguments.GetPrices): Unit = {
    val filePath = FundsMonthStatementFileReader.terminalFilePath(args.month)
    val file = new File(filePath)
    if (!file.exists()) {
      Exit.withErrorMessage { stream =>
        stream.println(s"O arquivo não existe: $filePath")
      }
    }

    val statement = FundsMonthStatementFileReader.read(args.month)
    val (_, recordSets) = StatementProcessor.process(args.month, statement, ensureLastDayOfMonth = true)

    val pricesToGet = getPricesToGet(recordSets)

    if (pricesToGet.isEmpty) {
      println("Nenhum preço a ser obtido")
    } else {
      println {
        val count = pricesToGet.length
        import BrWord._
        s"${identificado(count).toString.capitalize} ${preço(count).withCount} a ${ser(count)} ${obtido(count)}"
      }

      val fundsResultsFuture = Future.sequence(
        for ((fund, dates) <- pricesToGet.groupMap(_._1)(_._2))
          yield getFundSharePrices(fund, dates)
      )

      fundsResultsFuture
        .map { fundsResults =>
          val updatedStatement = mergeFundsResults(statement, fundsResults)

          updatedStatement match {
            case Some(updatedStatement) =>
              val rowsChunks = MonthStatementChunkMaker.makeChunks(updatedStatement)
              renameExistingFile(file)
              writeFile(file, rowsChunks)
            case None => println("Nenhum preço obtido")
          }
        }
        .recover(e => Exit.withErrorMessage(e.printStackTrace))
        .onComplete(_ => system.terminate())
    }
  }

  private def getPricesToGet(recordSets: Seq[RecordSet]): Seq[(String, LocalDate)] =
    for {
      recordSet <- recordSets
      (fund, record) <- recordSet.records.toSeq.sortBy { case (fund, _) => fund }
      if record.position.contains(Missing) || record.position.flatten.flatMap(_.note).exists(UpdatePriceTag.isIn)
    } yield (fund, recordSet.position.date)

  private sealed trait FundResult {
    def fund: String
  }
  private object FundResult {
    case class Success(fund: String, prices: Map[LocalDate, Option[Double]]) extends FundResult
    case class Failure(fund: String, operation: String, cause: Throwable) extends FundResult
  }

  private def getFundSharePrices(fund: String, dates: Seq[LocalDate]): Future[FundResult] = {
    def handleFailure[A](f: Future[A], operation: String)(g: A => Future[FundResult]): Future[FundResult] =
      f.transformWith {
        case Success(value) => g(value)
        case Failure(exception) => Future.successful(FundResult.Failure(fund, operation, exception))
      }

    handleFailure(carteiraGlobal.getFundId(fund), "getFundId") { fundId =>
      handleFailure(carteiraGlobal.getFundSharePrices(fundId), "getFundSharePrices") { obtainedPrices =>
        Future.successful(
          FundResult.Success(
            fund,
            prices = dates
              .map { date =>
                val price = obtainedPrices.get(date)
                date -> price
              }
              .toMap
          )
        )
      }
    }
  }

  private case class MergeState(statement: FundsStatement, count: Int)

  private def mergeFundsResults(statement: FundsStatement, fundsResults: Iterable[GetPricesOperation.FundResult]): Option[FundsStatement] = {
    val initialState = MergeState(statement, count = 0)

    val finalState = fundsResults.toSeq
      .sortBy(_.fund)
      .foldLeft(initialState) { (state, result) => mergeFundResult(result, state) }

    Option.when(finalState.count > 0)(finalState.statement)
  }

  private def mergeFundResult(result: FundResult, state: MergeState): MergeState =
    result match {
      case FundResult.Success(fund, prices) =>
        println(s"$fund:")
        prices.toSeq
          .sortBy(_._1)
          .foldLeft(state) { case (state, (date, price)) => mergePrice(fund, date, price, state) }

      case FundResult.Failure(fund, operation, cause) =>
        val status = highlighted(Code.Red, s"Falha ao tentar obter preços ($operation)")
        Console.err.println(s"$fund: $status")
        cause.printStackTrace()
        state
    }

  private def mergePrice(fund: String, date: LocalDate, price: Option[Double], state: MergeState): MergeState = {
    val statusPrefix = s"  $date: "

    def status(color: Code, text: String) = statusPrefix ++ color ++ text ++ Code.DefaultColor

    price match {
      case Some(price) =>
        state
          .modify(_.count).using(_ + 1)
          .modify(_.statement.entries.atOrElse(date, Map.empty))
          .using { entries =>
            val entry = entries.get(fund) match {
              case Some(entry) =>
                println(status(Code.Blue, "preço atualizado"))
                entry
                  .modify(_.sharePrice).setTo(price)
                  .modify(_.note).using(_.map(UpdatePriceTag.removeFrom).filter(_.nonEmpty))

              case None =>
                println(status(Code.Green, "preço adicionado"))
                FundsStatement.Entry(price, None, None)
            }
            entries + (fund -> entry)
          }
      case None =>
        println(statusPrefix ++ highlighted(Code.Yellow, "preço não disponível"))
        state
    }
  }

  private def renameExistingFile(file: File): Unit = {
    val now = LocalDateTime.now()
    val newFilePath = s"${file.getPath}.${SuffixTimestampFormat.format(now)}"
    file.renameTo(new File(newFilePath))

    println(s"Arquivo existente foi renomeado para $newFilePath")
  }

  private def writeFile(file: File, rowsChunks: Seq[Seq[TextAligner.Chunk]]): Unit = {
    val writer = new PrintWriter(new FileOutputStream(file))
    for (row <- TextAligner.alignAndRender(rowsChunks)) {
      writer.println(row)
    }
    writer.close()

    println(s"Arquivo regravado: ${file.getPath}")
  }

  private def highlighted(color: Code, text: AnsiString): AnsiString = {
    val FormatOn = AnsiString.escape(Code.Bright(Code.White), Code.BG(color), Code.Bold)
    val FormatOff = AnsiString.escape(Code.DefaultColor, Code.DefaultBgColor, Code.NormalIntensity)
    FormatOn ++ text ++ FormatOff
  }
}
