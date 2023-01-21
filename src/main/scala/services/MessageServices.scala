package services

import canoe.api.TelegramClient
import canoe.api.models.{ChatApi, Keyboard}
import canoe.models.Chat
import cats.syntax.functor._
import io.circe.Decoder
import canoe.models.messages.TelegramMessage
import canoe.syntax._
import cats.syntax.functor._

case class MessageServices[F[_]](services: RpsStorageServices[F]) {

  def sendTextMessageOnly(chat: ChatApi, messageText: String)(implicit tgClient: TelegramClient[F]) = {
    chat.send(messageText)
  }

  def sendTextMessageWithKeyboard(chat: ChatApi, messageText: String, keyboard: Keyboard)(implicit tgClient: TelegramClient[F]) = {
    chat.send(messageText, keyboard = keyboard)
  }
}