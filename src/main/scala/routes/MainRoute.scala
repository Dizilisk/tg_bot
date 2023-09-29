package routes

import cats.effect._
import fs2.io.net.Network
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.Logger
import cats.effect.Async
import cats.effect.unsafe.implicits.global
import cats.syntax.all._
import com.comcast.ip4s._
import doobie.Transactor
import doobie.util.transactor
import fs2.io.net.Network
import io.circe.{Encoder, Json}
import io.circe.literal.JsonStringContext
import io.circe.syntax.EncoderOps
import org.http4s.circe.CirceEntityCodec.{circeEntityDecoder, circeEntityEncoder}
import org.http4s.circe.jsonOf
import io.circe.parser.decode
import org.http4s.ember.client.EmberClientBuilder
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.implicits._
import org.http4s.server.middleware.Logger
import repositories.JsonRepo
import services.JsonProgram

object MainRoute {

  implicit val postDecoder: EntityDecoder[IO, HttpRequest] = jsonOf[IO, HttpRequest]

  implicit val jsonProgram: JsonProgram[IO] = ???

  val helloWorldService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")

    case message@POST -> Root / "bakaa" =>
      for {

        json <- message.as[HttpRequest]
        x = HttpResponse(json.firstName, json.lastName, json.greetings)
        res <- Ok(x.asJson)

      } yield (res)


    case request@POST -> Root / "baka" =>
      request.as[HttpRequest]
        .flatMap(user =>
          Ok(HttpResponse(user.firstName, user.lastName, user.greetings).asJson)
            .map(text => text)
        )
  }

  def run: IO[Nothing] = {
    for {

      client <- EmberClientBuilder.default[IO].build
//      transactor <- IO(Transactor.fromDriverManager[IO](
//        "org.postgresql.Driver",
//        s"""jdbc:postgresql://$address:$port/$name""",
//        "postgres",
//        "123456"
//      ))
//      repo = new JsonRepo[IO]()
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
