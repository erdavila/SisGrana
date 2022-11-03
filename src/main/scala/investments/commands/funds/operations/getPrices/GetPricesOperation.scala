package sisgrana
package investments.commands.funds.operations.getPrices

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.softwaremill.quicklens._
import investments.commands.funds.{Missing, MonthStatementChunkMaker, OperationArguments, RecordSet, StatementProcessor}
import investments.fileTypes.fundsMonthStatement.{FundsMonthStatementFileReader, FundsStatement}
import java.io.{File, FileOutputStream, PrintWriter}
import java.time.{LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import utils.AnsiString.Code
import utils.{AnsiString, Exit, HttpClient, TextAligner}

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
      println(s"Identificado(s) ${pricesToGet.length} preço(s) a ser(em) obtido(s)")

      val fundsResultsFuture = Future.sequence(
        for ((fund, dates) <- pricesToGet.groupMap(_._1)(_._2))
          yield getFundSharePrices(fund, dates)
      )

      fundsResultsFuture
        .map { fundsResults =>
          showResults(fundsResults)

          val obtainedPrices = filterObtainedPrices(fundsResults)
          if (obtainedPrices.isEmpty) {
            println("Nenhum preço obtido")
          } else {
            val updatedStatement = obtainedPrices.foldLeft(statement) { (statement, obtainedPrice) =>
              statement
                .modify(_.entries.at(obtainedPrice.date))
                .using(_ + (obtainedPrice.fund -> FundsStatement.Entry(obtainedPrice.price, None, None)))
            }
            val rowsChunks = MonthStatementChunkMaker.makeChunks(updatedStatement)

            renameExistingFile(file)
            writeFile(file, rowsChunks)
          }
        }
        .recover(e => Exit.withErrorMessage(e.printStackTrace))
        .onComplete(_ => system.terminate())
    }
  }

  private def getPricesToGet(recordSets: Seq[RecordSet]) = {
    for {
      recordSet <- recordSets
      (fund, record) <- recordSet.records.toSeq.sortBy { case (fund, _) => fund }
      if record.position.contains(Missing)
    } yield {
      (fund, recordSet.position.date)
    }
  }

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

  private def showResults(results: Iterable[FundResult]): Unit = {
    def highlighted(color: Code, text: AnsiString): AnsiString = {
      val FormatOn = AnsiString.escape(Code.Bright(Code.White), Code.BG(color), Code.Bold)
      val FormatOff = AnsiString.escape(Code.DefaultColor, Code.DefaultBgColor, Code.NormalIntensity)
      FormatOn ++ text ++ FormatOff
    }

    for (result <- results.toSeq.sortBy(_.fund)) {
      result match {
        case FundResult.Success(fund, prices) =>
          println(s"$fund:")
          for ((date, price) <- prices.toSeq.sortBy(_._1)) {
            val status = if (price.isDefined) {
              "preço obtido"
            } else {
              highlighted(Code.Yellow, "preço não disponível")
            }
            println(s"  $date: $status")
          }
        case FundResult.Failure(fund, operation, cause) =>
          val status = highlighted(Code.Red, s"Falha ao tentar obter preços ($operation)")
          Console.err.println(s"$fund: $status")
          cause.printStackTrace()
      }
    }
  }

  private case class ObtainedPrice(fund: String, date: LocalDate, price: Double)

  private def filterObtainedPrices(results: Iterable[GetPricesOperation.FundResult]): Seq[ObtainedPrice] =
    results.toSeq
      .collect {
        case FundResult.Success(fund, prices) =>
          for ((date, Some(price)) <- prices.toSeq)
            yield ObtainedPrice(fund, date, price)
      }
      .flatten

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
}
