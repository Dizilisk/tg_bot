package services

import canoe.api.TelegramClient
import canoe.api.models.ChatApi
import canoe.models.Chat
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{LoseGame, TopPlayers, UserInfo, WinGame}
import repositories.rawmodel.RpsStat
import services.Keyboards.{RpsKeyboard, SomeButton}

import scala.util.Random

class Program[F[_] : Monad](services: RpsStorageServices[F], msgServices: MessageServices[F]) {

  def rpsStart(prv: Boolean, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Камень-Ножницы-Бумага", RpsKeyboard.rpsGameStart(prv))
    } yield ()
  }

  def userReg(chat_id: Long, chat: ChatApi, userInfo: UserInfo)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- services.gameReg(chat_id, userInfo)
      _ <- services.gameStatReg(chat_id, userInfo)
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Выбирай", RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def userStat(prv: Boolean, chat_id: Long, chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      selfStat <- services.gameSelfStat(chat_id, id).map(_.map(
        rpsstat => s"@${rpsstat.username.getOrElse(rpsstat.first_name)} \nПобед: ${rpsstat.win_count} \nПоражения: ${rpsstat.lose_count}"
      ).getOrElse("Долбоеб"))
      _ <- msgServices.sendTextMessageWithKeyboard(chat, selfStat, RpsKeyboard.rpsGameStart(prv))
    } yield ()
  }

  def userTop(prv: Boolean, chat_id: Long, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {

    for {
      stat <- services.gameStat(chat_id)
      position = if (stat.isEmpty) {
        "Все долбоебы"
      } else
        stat.foldLeft("Top 10") {
          case (str, TopPlayers(place, userName, wins)) => s"$str \n$place. @$userName - $wins побед"
        }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, position, RpsKeyboard.rpsGameStart(prv))
    } yield ()
  }

  def userLeave(prv: Boolean, chat_id: Long, chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      del <- services.gameDel(chat_id, id).map {
        case Successful => "Слиток"
        case AlreadyDeleted => "Ты ещё не в игре"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, del, RpsKeyboard.rpsGameStart(prv))
    } yield ()
  }

  def backToMaimMenu(prv: Boolean, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    msgServices.sendTextMessageWithKeyboard(chat, "Камень-Ножницы-Бумага", RpsKeyboard.rpsGameStart(prv)).void
  }

  def rock(chat_id: Long, chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"Игрок (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил игрока (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def paper(chat_id: Long, chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"Игрок (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил игрока (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def scissors(chat_id: Long, chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"Игрок (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил игрока (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def greetings(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageOnly(chat, "Hello")
    } yield ()
  }

  def randomNumber(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageOnly(chat, s"${Random.nextInt(6)}")
    } yield ()
  }

  def rollButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Random", SomeButton.roll)
    } yield ()
  }

  def forwardButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Forward test", SomeButton.forward)
    } yield ()
  }

  def replyButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Reply test", SomeButton.reply)
    } yield ()
  }

  def linkButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Links", SomeButton.link)
    } yield ()
  }

  def payButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Donation", SomeButton.pay)
    } yield ()
  }

  def randomUserButton(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Random", SomeButton.randomUser)
    } yield ()
  }

  def other(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageOnly(chat, "Ты хуй")
    } yield ()
  }

  def echoBack(chat: ChatApi, any: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageOnly(chat, any)
    } yield ()
  }
}
