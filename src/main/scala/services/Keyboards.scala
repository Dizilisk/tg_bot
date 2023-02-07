package services

import canoe.api.models.Keyboard
import canoe.models.InlineKeyboardButton.{callbackData, switchInlineQuery, switchInlineQueryCurrentChat, url}
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.{InlineKeyboardButton, InlineKeyboardMarkup}

object Keyboards {

  object RpsKeyboard {

    def rpsGameStart(isPrivate: Boolean): Keyboard = if (isPrivate) rpsStartPrivate else rpsStart

    def rpsGameButton: Keyboard = rpsGameBtn

    val rpsGame: List[List[InlineKeyboardButton]] = List(List(callbackData("Камень", "Камень"),
      callbackData("Ножницы", "Ножницы"),
      callbackData("Бумага", "Бумага")),
      List(callbackData("Назад", "Назад")))
    val rpsGameBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(rpsGame))

    val rpsGameReg: List[List[InlineKeyboardButton]] = List(List(callbackData("Начать", "Рега"),
      callbackData("Статистика", "Стата")),
      List(callbackData("Топ 10", "Топ10"), callbackData("Покинуть игру", "Слиток")))
    val rpsStart: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(rpsGameReg))

    val rpsGameRegPrivate: List[List[InlineKeyboardButton]] = List(List(callbackData("Начать", "Рега"),
      callbackData("Покинуть игру", "Слиток")),
      List(callbackData("Статистика", "Стата")))
    val rpsStartPrivate: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(rpsGameRegPrivate))
  }

  object PidorButton {

    def pidorButton(isPrivate: Boolean): Keyboard = if (isPrivate) pidorBtnPrivate else pidorBtn

    val pidorGame: List[List[InlineKeyboardButton]] = List(List(callbackData("Выбрать пидора", "Пидор"),
      callbackData("Статистика", "ПидорСтата"), callbackData("Топ 10", "Топ10Пидоров")),
      List(callbackData("Истина", "Сергей"), callbackData("Я не пидор", "Ты пидор")))
    val pidorBtn: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(pidorGame))

    val pidorGamePrivate: List[List[InlineKeyboardButton]] = List(List(callbackData("Я пидор", "Пидор"),
      callbackData("Статистика", "ПидорСтата")),
      List(callbackData("Я не пидор", "Ты пидор")))
    val pidorBtnPrivate: Keyboard.Inline = Keyboard.Inline(InlineKeyboardMarkup(pidorGamePrivate))
  }
}