package routes

import io.circe._
import io.circe.generic.semiauto.deriveDecoder

case class HttpRequest(firstName: String, lastName: String, greetings: String)

object HttpRequest {
  implicit val decoder: Decoder[HttpRequest] = deriveDecoder
}
