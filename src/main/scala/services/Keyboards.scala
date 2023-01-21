package services

import canoe.api.models.Keyboard
import canoe.models.InlineKeyboardButton.{callbackData, switchInlineQuery, switchInlineQueryCurrentChat, url}
import canoe.models.InlineKeyboardMarkup.singleButton
import canoe.models.{InlineKeyboardButton, InlineKeyboardMarkup}

object Keyboards {

  object RpsKeyboard {

    def rpsGameStart: Keyboard = rpsStart

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

  }

  object SomeButton {

    def roll: Keyboard = rollingBtn

    def forward: Keyboard = forwardBtn

    def reply: Keyboard = replyBtn

    def link: Keyboard = linksBtn

    def randomUser: Keyboard = rollingBtn

    val frw: InlineKeyboardButton = switchInlineQuery("test", "")
    val swInlineQ: InlineKeyboardMarkup = singleButton(frw)
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
}