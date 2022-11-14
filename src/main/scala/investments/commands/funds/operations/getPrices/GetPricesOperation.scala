package sisgrana
package investments.commands.funds.operations.getPrices

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import com.softwaremill.quicklens._
import investments.commands.funds.{Missing, OperationArguments, RecordSet, StatementProcessor, StatementRewriter}
import investments.fileTypes.fundsMetadata.FundsMetadataFileReader
import investments.fileTypes.fundsMonthStatement.FundsStatement
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import utils.AnsiString.{Format, StringOps}
import utils.{AnsiString, BrWord, Exit, HttpClient}

object GetPricesOperation {
  private implicit lazy val system: ActorSystem[_] = ActorSystem(Behaviors.empty, "HttpClient")
  private lazy val carteiraGlobal = new CarteiraGlobal(new HttpClient)

  private lazy val FundsMetadataByShortName = FundsMetadataFileReader.read().map(fund => fund.shortName -> fund).toMap

  def execute(args: OperationArguments.GetPrices): Unit = {
    val rewriter = new StatementRewriter(args.month)
    val statement = rewriter.read()
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
            case Some(updatedStatement) => rewriter.rewrite(updatedStatement)
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
    case class Failure(fund: String, operation: String, cause: Throwable, dates: Seq[LocalDate]) extends FundResult
  }

  private def getFundSharePrices(fund: String, dates: Seq[LocalDate]): Future[FundResult] = {
    def handleFailure[A](f: Future[A], operation: String)(g: A => Future[FundResult]): Future[FundResult] =
      f.transformWith {
        case Success(value) => g(value)
        case Failure(exception) => Future.successful(FundResult.Failure(fund, operation, exception, dates))
      }

    val cnpj = FundsMetadataByShortName.get(fund) match {
      case Some(metadata) => metadata.cnpj
      case None => throw new Exception(s"""Não encontrado fundo com o nome curto "$fund"""")
    }

    handleFailure(carteiraGlobal.getFundIdByCnpj(cnpj), "getFundId") { fundId =>
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

      case FundResult.Failure(fund, operation, cause, dates) =>
        val status = highlighted(Format.Red, s"Falha ao tentar obter preços ($operation)")
        Console.err.println(s"$fund: $status para ${dates.mkString(", ")}")
        cause.printStackTrace()
        state
    }

  private def mergePrice(fund: String, date: LocalDate, price: Option[Double], state: MergeState): MergeState = {
    val statusPrefix = s"  $date: "

    def status(colorFormat: AnsiString.ColorFormat, text: String) = statusPrefix ++ colorFormat(text)

    price match {
      case Some(price) =>
        state
          .modify(_.count).using(_ + 1)
          .modify(_.statement.entries.atOrElse(date, Map.empty))
          .using { entries =>
            val entry = entries.get(fund) match {
              case Some(entry) =>
                println(status(Format.Blue, "preço atualizado"))
                entry
                  .modify(_.sharePrice).setTo(price)
                  .modify(_.note).using(_.map(UpdatePriceTag.removeFrom).filter(_.nonEmpty))

              case None =>
                println(status(Format.Green, "preço adicionado"))
                FundsStatement.Entry(price, None, None)
            }
            entries + (fund -> entry)
          }
      case None =>
        println(statusPrefix ++ highlighted(Format.Yellow, "preço não disponível"))
        state
    }
  }

  private def highlighted(colorFormat: AnsiString.ColorFormat, text: AnsiString): AnsiString =
    (Format.White.bright <|> colorFormat.bg <|> Format.Bold)(text)
}
