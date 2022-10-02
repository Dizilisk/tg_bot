import canoe.api._
import canoe.api.models.Keyboard
import canoe.models.InlineKeyboardButton.callbackData
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.{CallbackButtonSelected, Chat, InlineKeyboardButton, InlineKeyboardMarkup, KeyboardButton, PrivateChat, ReplyMarkup, Sticker, Update, User}
import canoe.models.messages.{AnimationMessage, StickerMessage, TelegramMessage, TextMessage}
import canoe.syntax._
import cats.syntax.functor._
import cats.{Applicative, Functor, Monad}
import cats.syntax.flatMap._
import cats.effect.{IO, IOApp}
import cats.implicits.toTraverseOps
import fs2.{Pipe, Stream}

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.util.matching.Regex
import scala.util.Random

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = ""
  val storage: Map[Long, String] = Map(0L -> "test")
  val numbers: Regex = "[1-9]".r
  val buf: ListBuffer[Sticker] = scala.collection.mutable.ListBuffer.empty[Sticker]
  val mp = scala.collection.mutable.Map.empty[Long, String]

  def run: IO[Unit] =
    Stream
      .resource(TelegramClient[IO](token))
      .flatMap(implicit client => Bot.polling[IO].follow(echos).through(answerCallbacks))
      .compile
      .drain

  def echos[F[_] : TelegramClient : Monad]: Scenario[F, Unit] =
    for {
      msg <- Scenario.expect(any)
      _ <- Scenario.eval(echoBack(msg))
    } yield ()

  def echoBack[F[_] : TelegramClient : Monad](msg: TelegramMessage): F[Unit] = msg match {
    case textMessage: TextMessage => textMessage.text match {
      //      case int =>  msg.chat.send(s"Вы ввели число $int").void
      case string => string.toLowerCase match {
        case "kb" => msg.chat.send("Testing", keyboard = keyboard2).void
        case "random" => msg.chat.send(s"${Random.nextInt(6)}").void
        case "map" => msg.chat.send(s"${mp.getOrElse(1L, "Нету")}").void
        case "hello" => msg.chat.send("Hi").void
        case "sticker" => if (buf.isEmpty) msg.chat.send("Пусто").void else msg.chat.send(RandomSticker(buf, Random)).void
        case "size" => msg.chat.send(s"Количество стикеров - ${buf.length}").void
        case "add" => {
          msg.chat.send("Пришли стикер для добавления").flatMap(
            _ => msg.chat.send("Пришли стикер")
            ).flatMap(
            _ => msg.chat.send("Добавлено \nКоличество стикеров - " + buf.length)).void
        }
        case "end" => throw new NoSuchElementException
        case "remove" => {
          if (buf.isEmpty) msg.chat.send("Нечего удалять").void
          else {
            msg.chat.send("Отправь стикер на удаление").void
          }
        }
        case _ => msg.chat.send("ti huy").void
      }
    }

    case animationMessage: AnimationMessage => msg.chat.send(animationMessage.animation).void
    case stickerMessage: StickerMessage => stickerMessage match {
      case add =>
        buf.addOne(add.sticker)
        msg.chat.send("Добавлено").void
      case remove => {
        msg.chat.send("Отправь стикер на удаление").void
      }
    }
    case keyboard: Keyboard => keyboard.chat.send("keyboard test").void
    case _ => msg.chat.send("Sorry! I can't echo that back.").void

  }

  def RandomSticker(list: ListBuffer[Sticker], random: Random): Sticker = {
    list(random.nextInt(list.length))
  }

  def addSticker(list: ListBuffer[Sticker], item: Sticker): ListBuffer[Sticker] = {
    list.addOne(item)
  }

  def removeSticker(list: ListBuffer[Sticker]): ListBuffer[Sticker] = {
    ???
  }

  def testt(a: Keyboard.Inline): Seq[Seq[String]] = {
   ???
  }

  def sendSticker[F[_] : TelegramClient : Monad](item: TelegramMessage): Sticker = {
    println("Отправь стикер")
    item match {
      case sticker: StickerMessage => sticker.sticker
      case _ => sendSticker(item)
      }
    }

  def answerCallbacks[F[_]: Monad: TelegramClient]: Pipe[F, Update, Update] =
    _.evalTap {
      case CallbackButtonSelected(_, query) =>
        query.data match {
          case Some(cbd) =>
            for {
              _ <- query.message.traverse(_.chat.send(cbd))
            } yield ()
          case _ => Applicative[F].unit
        }
      case _ => Applicative[F].unit
    }

  def addToMap(): String = {
    mp.update(1L, "name")
    "Добавлено"
  }

  val kbButton: InlineKeyboardButton = callbackData("test", "hello")
  val markup: InlineKeyboardMarkup = singleButton(kbButton)
  val keyboard: Keyboard.Inline = Keyboard.Inline(markup)
  val kbtest: List[List[InlineKeyboardButton]] = List(List(callbackData("a", addToMap()), callbackData("b", "B")), List(callbackData("c", "C"), callbackData("d", "D")))
  val keyboard2: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(kbtest))

}

