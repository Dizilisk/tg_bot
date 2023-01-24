import canoe.api._
import canoe.api.models.{ChatApi, Keyboard}
import canoe.models.InlineKeyboardButton.{callbackData, switchInlineQuery, switchInlineQueryCurrentChat, url}
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.messages.{AnimationMessage, StickerMessage, TelegramMessage, TextMessage}
import canoe.models._
import canoe.syntax._
import cats.Applicative
import cats.data.EitherT
import cats.effect.{Async, IO, IOApp, LiftIO, Sync}
import cats.free.Free
import cats.implicits.toTraverseOps
import cats.syntax.flatMap._
import cats.syntax.applicative._
import cats.syntax.functor._
import domain.{LoseGame, TopPlayers, UserInfo, WinGame}
import doobie.Transactor
import doobie.free.connection
import doobie.implicits._
import fs2.{Pipe, Stream}
import repositories.RpsRepo
import repositories.rawmodel.Allstat

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Random, Try}
import services.{AlreadyDeleted, Keyboards, MessageServices, Program, RpsStorageServices, SqlDbEvolution, Successful}

import scala.concurrent.ExecutionContext.Implicits.global

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = "5668930687:AAEzCyL4Y-cQoLph4EpW_y_7JX7c4SMA9TQ"
  val storage: Map[Long, String] = Map(0L -> "test")
  val numbers: Regex = "[1-9]".r
  val buf: ListBuffer[Sticker] = scala.collection.mutable.ListBuffer.empty[Sticker]
  val mp = scala.collection.mutable.Map.empty[Long, String]

  def run: IO[Unit] = for {
    transactor <- IO(Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:8888/testdb",
      "postgres",
      "123456"
    ))
    _ <- IO.fromFuture(IO(SqlDbEvolution().runEvolutions()))
    repo = new RpsRepo[IO](transactor)
    service = new RpsStorageServices[IO](repo)
    msgService = new MessageServices[IO](service)
    program = new Program[IO](service, msgService)
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
      case PrivateChat(id, username, firstName, lastName) => true
      case _ => false
    }
    msg match {
    case textMessage: TextMessage => textMessage.text match {

      case "/hello" => program.greetings(msg.chat)
      case "/random" => program.randomNumber(msg.chat)
      case "/roll" => program.randomUserButton(msg.chat)
      case "/reply" => program.replyButton(msg.chat)
      case "/forward" => program.forwardButton(msg.chat)
      case "/link" => program.linkButton(msg.chat)
      case "/pay" => program.payButton(msg.chat)
      case "/game" => program.rpsStart(isPrivate, msg.chat)
      case _ => program.other(msg.chat)
    }

    case animationMessage: AnimationMessage => msg.chat.send(animationMessage.animation).void
    case stickerMessage: StickerMessage => msg.chat.send(stickerMessage.sticker).void
    case _ => msg.chat.send("Sorry! I can't echo that back.").void

  }
  }

  def getDB[F[_] : TelegramClient : Sync](trans: Transactor[F]): F[List[(String, String)]] = {
    val request = sql"select pidor_id, name from testshema.pidordnya"
      .query[(String, String)]
      .to[List]
    val get: F[List[(String, String)]] = request.transact(trans)
    get
  }

  def answerCallbacks[F[_] : Sync : TelegramClient](program: Program[F]): Pipe[F, Update, Update] = {

    _.evalTap {
      case CallbackButtonSelected(_, query) =>
        query.data match {
          case Some(cbd) =>

            val chat: Chat = query.message.get.chat
            val isPrivate = chat match {
              case PrivateChat(id, username, firstName, lastName) => true
              case _ => false
            }
            val id = query.from.id
            val username = query.from.username
            val firstName = query.from.firstName
            val lastName = query.from.lastName
            val userInfo = UserInfo(id, username, firstName, lastName)
            cbd match {
              case "Добавлено" =>
                addToMap(id, firstName)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                } yield ()
              case "Удалено" =>
                removeFromMap(id, firstName)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                } yield ()
              case "Рандом" =>
                for {
                  _ <- query.message.traverse(_.chat.send(random()))
                } yield ()
              case "Список" =>
                for {
                  _ <- query.message.traverse(_.chat.send(showList()))
                } yield ()


              case "Рега" => program.userReg(chat.id, chat, userInfo)

              case "Камень" => program.rock(chat.id, chat, id, cbd)

              case "Ножницы" => program.scissors(chat.id, chat, id, cbd)

              case "Бумага" => program.paper(chat.id, chat, id, cbd)

              case "Стата" => program.userStat(isPrivate, chat.id, chat, id)

              case "Топ10" => program.userTop(isPrivate, chat.id, chat)

              case "Слиток" => program.userLeave(isPrivate, chat.id, chat, id)

              case "Назад" => program.backToMaimMenu(isPrivate, chat)

              case _ => program.echoBack(chat, cbd)
            }
          case _ => Applicative[F].unit
        }
      case _ => Applicative[F].unit
    }
  }

  def addToMap(id: Long, name: String): String = {
    mp.update(id, name)
    "Добавлено"
  }

  def addToDB[F[_] : Sync](id: Long, name: String, trans: Transactor[F]): F[Int] = {
    sql"insert into testshema.pidordnya values ($id, $name)"
      .update
      .run.transact(trans)
  }

  def delFromDB[F[_] : Sync](id: Long, trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.pidordnya where pidor_id = $id"
      .update
      .run.transact(trans)
  }

  def delAllDB[F[_] : Sync](trans: Transactor[F]): F[Int] = {
    sql"delete from testshema.pidordnya"
      .update
      .run.transact(trans)
  }

  def removeFromMap(id: Long, name: String): String = {
    if (mp.contains(id)) {
      mp.remove(id).get
      "Удалено"
    }
    else name + " не найден"

  }

  def showList(): String = {
    if (mp.isEmpty) "Пусто"
    else {
      val x: Seq[(Long, String)] = mp.toSeq
      val y = for {
        name <- x._2F
      } yield name
      y.mkString("Список: (", ", ", ")")
    }
  }

  def random(): String = {
    if (mp.isEmpty) "Пусто"
    else {
      val x: Seq[(Long, String)] = mp.toSeq
      val y = for {
        name <- x._2F
      } yield name
      "@" + y(Random.nextInt(y.length))
    }
  }
}