package routes

import cats.effect._
import io.circe._
import io.circe.generic.semiauto.deriveDecoder
import io.circe.literal._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.implicits._

case class HttpRequest(firstName: String, lastName: String)

object HttpRequest {
  implicit val decoder: Decoder[HttpRequest] = deriveDecoder
}
