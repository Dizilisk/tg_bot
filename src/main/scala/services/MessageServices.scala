package services

import canoe.api.TelegramClient
import canoe.api.models.{ChatApi, Keyboard}
import canoe.models.messages.TextMessage
import canoe.syntax._

case class MessageServices[F[_]](services: RpsStorageServices[F]) {

  def sendTextMessageOnly(chat: ChatApi, messageText: String)(implicit tgClient: TelegramClient[F]): F[TextMessage] = {
    chat.send(messageText)
  }

  def sendTextMessageWithKeyboard(chat: ChatApi, messageText: String, keyboard: Keyboard)(implicit tgClient: TelegramClient[F]): F[TextMessage] = {
    chat.send(messageText, keyboard = keyboard)
  }
}