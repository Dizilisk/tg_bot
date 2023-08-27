package routes

import cats.effect._
import fs2.io.net.Network
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger
import cats.effect.Async
import cats.syntax.all._
import com.comcast.ip4s._
import fs2.io.net.Network
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder
import org.http4s.circe.jsonOf
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger

object MainRoute {

  implicit val postDecoder: EntityDecoder[IO, HttpRequest] = jsonOf[IO, HttpRequest]

  val helloWorldService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
    case request@ POST -> Root / "baka" =>
      for {
        // Decode a User request
        user <- request.as[HttpRequest]
        // Encode a hello response
        resp <- Ok(HttpResponse(user.firstName, user.lastName).asJson)
      } yield (resp)
  }

  def run: IO[Nothing] = {
    for {
      client <- EmberClientBuilder.default[IO].build

      // Combine Service Routes into an HttpApp.
      // Can also be done via a Router if you
      // want to extract segments not checked
      // in the underlying routes.
      httpApp = (
        helloWorldService
        ).orNotFound

      // With Middlewares in place
      finalHttpApp = Logger.httpApp(true, true)(httpApp)

      _ <-
        EmberServerBuilder.default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(finalHttpApp)
          .build
    } yield ()
  }.useForever

}
