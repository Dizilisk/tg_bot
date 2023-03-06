import canoe.api._
import canoe.models.messages.{TelegramMessage, TextMessage}
import canoe.models._
import canoe.syntax._
import cats.Applicative
import cats.effect.{IO, IOApp, Sync}
import cats.syntax.applicative._
import domain.UserInfo
import doobie.Transactor
import fs2.{Pipe, Stream}
import repositories.{PidorRepo, RpsRepo}
import services.{MessageServices, PidorStorageServices, Program, RpsStorageServices, SqlDbEvolution}

import scala.concurrent.ExecutionContext.Implicits.global

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = ""

  def run: IO[Unit] = for {
    transactor <- IO(Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "",
      "",
      ""
    ))
    _ <- IO.fromFuture(IO(SqlDbEvolution().runEvolutions()))
    rpsRepo = new RpsRepo[IO](transactor)
    service = new RpsStorageServices[IO](rpsRepo)
    pidorRepo = new PidorRepo[IO](transactor)
    pidor = new PidorStorageServices[IO](pidorRepo)
    msgService = new MessageServices[IO](service)
    program = new Program[IO](service, pidor, msgService)
    result <- Stream
      .resource(TelegramClient[IO](token))
      .flatMap(implicit client => Bot.polling[IO].follow(echos(program)).through(answerCallbacks(program)))
      .compile
      .drain
  } yield result

  def echos[F[_] : TelegramClient : Sync](program: Program[F]): Scenario[F, Unit] =
    for {
      msg <- Scenario.expect(any)
      _ <- Scenario.eval(echoBack(msg, program))
    } yield ()

  def echoBack[F[_] : TelegramClient : Sync](msg: TelegramMessage, program: Program[F]): F[Unit] = {
    val getChat = msg.chat
    val isPrivate = getChat match {
      case PrivateChat(_, _, _, _) => true
      case _ => false
    }
    msg match {
      case textMessage: TextMessage if isPrivate =>
        textMessage.text match {
          case "/hello" => program.greetings(msg.chat)
          case "/random" => program.randomNumber(msg.chat)
          case "/game" => program.rpsStart(isPrivate, msg.chat)
          case "/pidor" => program.pidorStart(isPrivate, msg.chat)
          case _ => ().pure[F]
        }
      case textMessage: TextMessage if !isPrivate =>
        textMessage.text match {
          case "/hello@bakaebaka_bot" => program.greetings(msg.chat)
          case "/random@bakaebaka_bot" => program.randomNumber(msg.chat)
          case "/game@bakaebaka_bot" => program.rpsStart(isPrivate, msg.chat)
          case "/pidor@bakaebaka_bot" => program.pidorStart(isPrivate, msg.chat)
          case _ => ().pure[F]
        }
      case _ => ().pure[F]

    }
  }

  def answerCallbacks[F[_] : Sync : TelegramClient](program: Program[F]): Pipe[F, Update, Update] = {

    _.evalTap {
      case CallbackButtonSelected(_, query) =>
        query.data match {
          case Some(cbd) =>

            val chat: Chat = query.message.get.chat
            val isPrivate = chat match {
              case PrivateChat(_, _, _, _) => true
              case _ => false
            }
            val id = query.from.id
            val username = query.from.username
            val firstName = query.from.firstName
            val lastName = query.from.lastName
            val userInfo = UserInfo(id, username, firstName, lastName)
            cbd match {

              case "Рега" => program.userReg(chat.id, chat, userInfo)

              case "Камень" => program.rock(chat.id, chat, userInfo, id, cbd)

              case "Ножницы" => program.scissors(chat.id, chat, userInfo, id, cbd)

              case "Бумага" => program.paper(chat.id, chat, userInfo, id, cbd)

              case "Стата" => program.userStat(isPrivate, chat.id, chat, id)

              case "Топ10" => program.userTop(isPrivate, chat.id, chat)

              case "Слиток" => program.userLeave(isPrivate, chat.id, chat, id)

              case "Назад" => program.backToMaimMenu(isPrivate, chat)

              case "Пидор" => program.pidorGame(isPrivate, chat.id, chat, userInfo)

              case "ПидорСтата" => program.pidorSelfStat(isPrivate, chat.id, chat, id)

              case "Топ10Пидоров" => program.pidorStat(isPrivate, chat.id, chat)

              case "Я не пидор" => program.pidorLeave(isPrivate, chat.id, chat, id)

              case _ => program.echoBack(chat, cbd)
            }
          case _ => Applicative[F].unit
        }
      case _ => Applicative[F].unit
    }
  }
}
