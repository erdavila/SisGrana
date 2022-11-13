package sisgrana
package investments.commands.funds.operations.getPrices

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json.DefaultJsonProtocol._
import spray.json.{DeserializationException, JsString, JsValue, RootJsonFormat}
import utils.HttpClient

class CarteiraGlobal(httpClient: HttpClient) {
  private val BaseUrl = "https://next.carteiraglobal.com"

  private lazy val buildId = {
    val RE = """"/_next/static/([^/]+)/_buildManifest\.js"""".r.unanchored
    for (html <- httpClient.get[String](BaseUrl))
      yield RE.findFirstMatchIn(html).get.group(1)
  }

  def getFundId(fundName: String): Future[Long] = {
    case class Request(query: String, variables: Map[String, String])
    case class Response(data: Map[String, Seq[DataItem]])
    case class DataItem(id: Long, name: String)

    implicit val requestFormat: RootJsonFormat[Request] = jsonFormat2(Request)
    implicit val dataItemFormat: RootJsonFormat[DataItem] = jsonFormat2(DataItem)
    implicit val responseFormat: RootJsonFormat[Response] = jsonFormat1(Response)

    val request = Request(
      query =
        """query ($input: String) {
          |  getSearchConhecaAutocomplete(input: $input) {
          |    name
          |    id
          |  }
          |}
          |""".stripMargin,
      variables = Map("input" -> fundName),
    )

    for (response <- httpClient.post[Request, Response](s"$BaseUrl/api/graphql", request))
      yield response.data("getSearchConhecaAutocomplete")
        .collectFirst { case DataItem(id, `fundName`) => id }
        .getOrElse(throw new Exception("Fund not found"))
  }

  def getFundSharePrices(fundId: Long): Future[Map[LocalDate, Double]] = {
    case class Response(pageProps: PageProps)
    case class PageProps(dataTable: DataTable)
    case class DataTable(rows: Seq[Row])
    case class Row(date_report: LocalDate, quote_value: Double)

    implicit val localDateFormat: RootJsonFormat[LocalDate] = new RootJsonFormat[LocalDate] {
      override def write(obj: LocalDate): JsValue = JsString(obj.toString)
      override def read(json: JsValue): LocalDate = json match {
        case JsString(value) => LocalDate.parse(value)
        case _ => throw DeserializationException(s"JSON value should be string to be deserialized to LocalDate: $json")
      }
    }
    implicit val rowFormat: RootJsonFormat[Row] = jsonFormat2(Row)
    implicit val dataTableFormat: RootJsonFormat[DataTable] = jsonFormat1(DataTable)
    implicit val pagePropsFormat: RootJsonFormat[PageProps] = jsonFormat1(PageProps)
    implicit val responseFormat: RootJsonFormat[Response] = jsonFormat1(Response)

    for {
      buildId <- buildId
      response <- httpClient.get[Response](s"$BaseUrl/_next/data/$buildId/conheca/fundos-de-investimento/$fundId.json")
    } yield response.pageProps.dataTable.rows
      .map(row => row.date_report -> row.quote_value)
      .toMap
  }
}
