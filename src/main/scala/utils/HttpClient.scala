package sisgrana
package utils

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshalling.{Marshal, Marshaller}
import akka.http.scaladsl.model.{HttpEntity, HttpMethod, HttpMethods, HttpRequest, HttpResponse, RequestEntity}
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import scala.concurrent.{ExecutionContext, Future}

class HttpClient(implicit system: ActorSystem[_]) {
  private implicit val executionContext: ExecutionContext = system.executionContext

  def get[Response](
    uri: String
  )(implicit unmarshaller: Unmarshaller[HttpResponse, Response]): Future[Response] =
    doRequest[Response](HttpMethods.GET, uri, HttpEntity.Empty)

  def post[Request, Response](
    uri: String,
    request: Request,
  )(implicit marshaller: Marshaller[Request, RequestEntity], unmarshaller: Unmarshaller[HttpResponse, Response]): Future[Response] =
    for {
      entity <- Marshal(request).to[RequestEntity]
      response <- doRequest[Response](HttpMethods.POST, uri, entity)
    } yield response

  private def doRequest[Response](
    method: HttpMethod,
    uri: String,
    entity: RequestEntity,
  )(implicit unmarshaller: Unmarshaller[HttpResponse, Response]): Future[Response] = {
    val httpRequest = HttpRequest(
      method = method,
      uri = uri,
      entity = entity,
    )

    for {
      httpResponse <- Http().singleRequest(httpRequest)
      response <- Unmarshal(httpResponse).to[Response]
    } yield response
  }
}
