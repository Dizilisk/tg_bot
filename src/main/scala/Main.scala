import canoe.api._
import canoe.api.models.Keyboard
import canoe.models.{Chat, Sticker}
import canoe.models.messages.{AnimationMessage, StickerMessage, TelegramMessage, TextMessage}
import canoe.syntax._
import cats.syntax.functor._
import cats.{Functor, Monad}
import cats.syntax.flatMap._
import cats.effect.{IO, IOApp}
import fs2.Stream

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.Random

/** Example of echos bot that will answer to you with the message you've sent to him
 */
object Main extends IOApp.Simple {
  val token: String = ""
  val storage: Map[Int, String] = Map(0 -> "test")
  val numbers: Regex = "[1-9]".r
  val buf: ListBuffer[Sticker] = scala.collection.mutable.ListBuffer.empty[Sticker]

  def run: IO[Unit] =
    Stream
      .resource(TelegramClient[IO](token))
      .flatMap(implicit client => Bot.polling[IO].follow(echos))
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
        case "random" => msg.chat.send(s"${Random.nextInt(6)}").void
        case "hello" => msg.chat.send("Hi").void
        case "sticker" => if (buf.isEmpty) msg.chat.send("Пусто").void else msg.chat.send(RandomSticker(buf, Random)).void
        case "size" => msg.chat.send(s"Количество стикеров - ${buf.length}").void
        case "add" => {
          msg.chat.send("Пришли стикер для добавления").flatMap(
            _ => {
              case sticker: StickerMessage => addSticker(buf, sticker.sticker)
              case _ => msg.chat.send("Пришли стикер")
            }).flatMap(
            _ => msg.chat.send("Добавлено \nКоличество стикеров - " + buf.length))
        }
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

  def sendSticker[F[_] : TelegramClient : Monad](item: TelegramMessage): Sticker = {
    println("Отправь стикер")
    item match {
      case sticker: StickerMessage => sticker.sticker
      case _ => sendSticker(item)
      }
    }
}

