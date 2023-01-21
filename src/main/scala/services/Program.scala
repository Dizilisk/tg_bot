package services

import canoe.api.TelegramClient
import canoe.api.models.ChatApi
import cats.Monad
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{LoseGame, TopPlayers, WinGame}
import repositories.rawmodel.Allstat
import services.Keyboards.{RpsKeyboard, SomeButton}

import scala.util.Random

class Program[F[_] : Monad](services: RpsStorageServices[F], msgServices: MessageServices[F]) {

  def rpsStart(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Камень-Ножницы-Бумага", RpsKeyboard.rpsGameStart)
    } yield ()
  }

  def userReg(chat: ChatApi, name: String, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- services.gameReg(name, id)
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Выбирай", RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def userStat(chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      selfStat <- services.gameSelfStat(id).map(_.get).map {
        allstat: Allstat => s"@${allstat.player_name} \nПобедs: ${allstat.win_counter} \nПоражения: ${allstat.lose_counter}"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, selfStat, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def userTop(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      stat <- services.gameStat
      position = stat.foldLeft("") {
        case (str, TopPlayers(place, userName, wins)) => s"$str \n$place. @$userName - $wins побед"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, position, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def userLeave(chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      del <- services.gameDel(id).map {
        case Successful => "Слиток"
        case AlreadyDeleted => "Ты ещё не в игре"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, del, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def backToMaimMenu(chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    msgServices.sendTextMessageWithKeyboard(chat, "Камень-Ножницы-Бумага", RpsKeyboard.rpsGameStart).void
  }

  def rock(chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(id, choose).map {
        case winGame: WinGame => s"Игрок (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил игрока (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def paper(chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(id, choose).map {
        case winGame: WinGame => s"Игрок (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил игрока (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def scissors(chat: ChatApi, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(id, choose).map {
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
    for  {
      _ <- msgServices.sendTextMessageOnly(chat, any)
    } yield ()
  }
}
