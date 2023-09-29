package routes

import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder

case class HttpResponse(nameFirst: String, nameLast: String, message: String)

object HttpResponse {

  implicit val encoder: Encoder.AsObject[HttpResponse] = deriveEncoder

}
