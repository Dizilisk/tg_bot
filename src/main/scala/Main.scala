import canoe.api._
import canoe.api.models.Keyboard
import canoe.models.InlineKeyboardButton.{callbackData, pay, switchInlineQuery, switchInlineQueryCurrentChat, url}
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.{CallbackButtonSelected, CallbackQuery, Chat, InlineKeyboardButton, InlineKeyboardMarkup, KeyboardButton, PrivateChat, ReplyMarkup, Sticker, Update, User}
import canoe.models.messages.{AnimationMessage, StickerMessage, TelegramMessage, TextMessage}
import canoe.syntax._
import cats.syntax.functor._
import cats.{Applicative, Functor, Monad}
import cats.effect.{IO, IOApp, Sync}
import cats.implicits.toTraverseOps
import doobie.implicits.toSqlInterpolator
import doobie.{ConnectionIO, Transactor}
import fs2.{Pipe, Stream}
import doobie.implicits._
import cats.syntax.flatMap._

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.{Random, Try}

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = ""
  val storage: Map[Long, String] = Map(0L -> "test")
  val numbers: Regex = "[1-9]".r
  val buf: ListBuffer[Sticker] = scala.collection.mutable.ListBuffer.empty[Sticker]
  val mp = scala.collection.mutable.Map.empty[Long, String]

  def run: IO[Unit] = for {
    transactor <- IO (Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",
      "jdbc:postgresql://localhost:8888/testdb",
      "postgres",
      "123456"
    ))
    result <- Stream
      .resource(TelegramClient[IO](token))
      .flatMap(implicit client => Bot.polling[IO].follow(echos(transactor)).through(answerCallbacks(transactor)))
      .compile
      .drain
  } yield result

  def echos[F[_] : TelegramClient : Sync](trans: Transactor[F]): Scenario[F, Unit] =
    for {
      msg <- Scenario.expect(any)
      _ <- Scenario.eval(echoBack(msg, trans))
    } yield ()

  def echoBack[F[_] : TelegramClient : Sync](msg: TelegramMessage, trans: Transactor[F]): F[Unit] = msg match {
    case textMessage: TextMessage => textMessage.text match {
      case "database" => {
        for {
          unwrapGet <- getDB(trans)
          _ <- msg.chat.send("@" + s"${unwrapGet._2F.head}").void
        } yield ()
      }
      case "adddb" =>
        test(trans)
        msg.chat.send("added").void
      case "deldb" => {
        delAllDB(trans).void
      }
      case "showl" => for {
        ss <- getDB(trans)
        _ <- msg.chat.send(s"${ss}").void
      } yield ()
      case "/hello" => msg.chat.send("Hi").void
      case "/roll" => msg.chat.send("Random", keyboard = rollingBtn).void
      case "/random" => msg.chat.send(s"${Random.nextInt(6)}").void
      case "/reply" => msg.chat.send("ReplyTest", keyboard = replyBtn).void
      case "/forward" => msg.chat.send("ForwardTest", keyboard = forwardBtn).void
      case "/link" => msg.chat.send("LinksTest", keyboard = linksBtn).void
      case "/pay" => msg.chat.send("Donation", keyboard = pay).void
      case _ => msg.chat.send("ti huy").void
    }

    case animationMessage: AnimationMessage => msg.chat.send(animationMessage.animation).void
    case stickerMessage: StickerMessage => msg.chat.send(stickerMessage.sticker).void
    case _ => msg.chat.send("Sorry! I can't echo that back.").void

  }

  def getDB[F[_] : TelegramClient : Sync](trans: Transactor[F]): F[List[(String, String)]] = {
    val request = sql"select pidor_id, name from testshema.pidordnya"
      .query[(String, String)]
      .to[List]
    val get: F[List[(String, String)]] = request.transact(trans)
    get
  }

  def answerCallbacks[F[_] : Sync : TelegramClient](trans: Transactor[F]): Pipe[F, Update, Update] =
    _.evalTap {
      case CallbackButtonSelected(_, query) =>
        query.data match {
          case Some(cbd) =>
            val id = query.from.id
            val name = if (query.from.username.isEmpty) {
              Try(query.from.firstName).get
            }
            else query.from.username.get
            cbd match {
              case "Добавлено" =>
                addToMap(id, name)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                  _ <- addToDB(id, name, trans)
                } yield ()
              case "Удалено" =>
                removeFromMap(id, name)
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                  _ <- delFromDB(id, trans)
                } yield ()
              case "Рандом" =>
                for {
                  _ <- query.message.traverse(_.chat.send(random()))
                } yield ()
              case "Список" =>
                for {
                  _ <- query.message.traverse(_.chat.send(showList()))
                } yield ()
              case _ =>
                for {
                  _ <- query.message.traverse(_.chat.send(cbd))
                } yield ()
            }
          case _ => Applicative[F].unit
        }
      case _ => Applicative[F].unit
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
  def test[F[_] : Sync](trans: Transactor[F]): F[Int] = {
    val x: Long = 11111111
    val y: String = "testname"
    sql"insert into testshema.pidordnya values ($x, $y)"
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

  def randomFromDB[F[_] : Sync](trans: Transactor[F]): F[List[String]] = {
    val request = sql"select name from testshema.pidordnya"
      .query[String]
      .to[List]
    val get = request.transact(trans)
    val x: F[List[String]] = for {
      s <- get
    } yield s
    val y: F[List[String]] = x
    y
  }

  val forward: InlineKeyboardButton = switchInlineQuery("test", "")
  val swInlineQ: InlineKeyboardMarkup = singleButton(forward)
  val forwardBtn: Keyboard.Inline = Keyboard.Inline(swInlineQ)

  val pay: Keyboard.Inline = Keyboard.Inline(
    InlineKeyboardMarkup.singleButton(
      callbackData("pay", "Work in progress...")))

  val buttontest: InlineKeyboardButton = switchInlineQueryCurrentChat("test", "")
  val swinlineQCC: InlineKeyboardMarkup = singleButton(buttontest)
  val replyBtn: Keyboard.Inline = Keyboard.Inline(swinlineQCC)


  val kbURL: InlineKeyboardButton = url("Google", "https://www.google.com/")
  val markup: InlineKeyboardMarkup = singleButton(kbURL)


  val links: List[List[InlineKeyboardButton]] = List(List(url("Google", "https://www.google.com/"),
    url("Youtube", "https://www.youtube.com/")),
    List(url("Pornhub", "https://rt.pornhub.com")))
  val linksBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(links))


  val kbtest: List[List[InlineKeyboardButton]] = List(List(callbackData("Добавить", "Добавлено"),
    callbackData("Удалить", "Удалено")),
    List(callbackData("Рандом", "Рандом"),
      callbackData("Список", "Список")))
  val rollingBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(kbtest))


}

