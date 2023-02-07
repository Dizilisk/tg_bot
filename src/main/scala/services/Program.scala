package services

import canoe.api.TelegramClient
import canoe.api.models.ChatApi
import canoe.models.Chat
import cats.Monad
import cats.implicits.toTraverseOps
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{LoseGame, TopPidors, TopPlayers, UserInfo, WinGame}
import services.Keyboards.{PidorButton, RpsKeyboard}

import java.time.{LocalDate, ZoneId}
import scala.util.Random

class Program[F[_] : Monad](services: RpsStorageServices[F], pidor: PidorStorageServices[F], msgServices: MessageServices[F]) {

  def rpsStart(prv: Boolean, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Камень-Ножницы-Бумага", RpsKeyboard.rpsGameStart(prv))
    } yield ()
  }

  def userReg(chat_id: Long, chat: ChatApi, userInfo: UserInfo)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- services.gameReg(userInfo)
      _ <- services.gameStatReg(chat_id, userInfo)
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Выбирай", RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def userStat(prv: Boolean, chat_id: Long, chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      selfStat <- services.gameSelfStat(chat_id, id).map(_.map(
        rpsstat => s"${
          rpsstat.username match {
            case Some(value) => "@" + value
            case None => rpsstat.first_name + " " + rpsstat.last_name.getOrElse(" ")
          }
        } \nПобед: ${rpsstat.win_count} \nПоражения: ${rpsstat.lose_count}"
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
          case (str, TopPlayers(place, userName, first_name, wins)) => s"$str \n$place. ${
            userName match {
              case Some(value) => "@" + value
              case None => first_name
            }
          } - $wins побед"
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

  def rock(chat_id: Long, chat: ChatApi, user: UserInfo, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил ${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def paper(chat_id: Long, chat: ChatApi, user: UserInfo, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил ${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def scissors(chat_id: Long, chat: ChatApi, user: UserInfo, id: Long, choose: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      res <- services.rpsGame(chat_id, id, choose).map {
        case winGame: WinGame => s"${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${winGame.player}) победил противника (${winGame.bot})"
        case loseGame: LoseGame => s"Противник (${loseGame.bot}) победил ${
          user.username match {
            case Some(value) => "@" + value
            case None => user.firstName
          }
        } (${loseGame.player})"
        case _ => "Ничья"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, res, RpsKeyboard.rpsGameButton)
    } yield ()
  }

  def pidorStart(prv: Boolean, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageWithKeyboard(chat, "Пидор дня", PidorButton.pidorButton(prv))
    } yield ()
  }

  def pidorReg(chat_id: Long, userInfo: UserInfo)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- services.gameReg(userInfo)
      _ <- pidor.pidorReg(chat_id, userInfo)
    } yield ()
  }

  def pidorGame(prv: Boolean, chat_id: Long, chat: ChatApi, userInfo: UserInfo)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    val time = LocalDate.now(ZoneId.of("UTC"))
    for {
      _ <- pidorReg(chat_id, userInfo)
      maybeWinner <- pidor.pidorGet(chat_id, time)
      responseText <- maybeWinner match {
        case Some(existingPidor) =>
          println(existingPidor)
          pidor.pidorSelfStat(existingPidor.chat_id, existingPidor.user_id)
          .map {test =>
            test.fold("Никого")(name => name.username match {
              case Some(value) => "Пидор дня @" + value
              case None => "Пидор дня " + name.first_name
            })}
        case None => selectPidorOfTheDay(chat_id, time).map(_.fold("Никого")(name =>
          name.username match {
            case Some(value) => "Пидор дня @" + value
            case None => "Пидор дня " + name.first_name
          }
        ))
      }

      _ <- msgServices.sendTextMessageWithKeyboard(chat, responseText, PidorButton.pidorButton(prv))
    } yield ()
  }

  private def selectPidorOfTheDay(chat_id: Long, time: LocalDate): F[Option[TopPidors]] = {
    for {
      pidorList <- pidor.pidorStat(chat_id)
      random = new Random
      select = Option(pidorList(random.nextInt(pidorList.length)))
      _ <- select.traverse((current: TopPidors) => pidor.updatePidorOfTheDay(chat_id, current.user_id, time))
    } yield select
  }

  def pidorStat(prv: Boolean, chat_id: Long, chat: ChatApi)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      stat <- pidor.pidorStat(chat_id)
      position = if (stat.isEmpty) {
        "Пидоров нету, но вы все равно пидоры"
      } else
        stat.foldLeft("Top 10") {
          case (str, TopPidors(place, _, username, first_name, pidor_count)) => s"$str \n$place. ${
            username match {
              case Some(value) => "@" + value
              case None => first_name
            }
          } был пидором $pidor_count раз"
        }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, position,  PidorButton.pidorButton(prv))
    } yield ()
  }

  def pidorSelfStat(prv: Boolean, chat_id: Long, chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      selfStat <- pidor.pidorSelfStat(chat_id, id).map(_.map(
        pidorstat => s"Ты был пидором ${pidorstat.pidor_count} раз"
      ).getOrElse("Сначала добавься в пидоры"))
      _ <- msgServices.sendTextMessageWithKeyboard(chat, selfStat, PidorButton.pidorButton(prv))
    } yield ()
  }

  def pidorLeave(prv: Boolean, chat_id: Long, chat: ChatApi, id: Long)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      del <- pidor.pidorDel(chat_id, id).map {
        case Successful => "Ты все равно пидор"
        case AlreadyDeleted => "Сначала добавься в пидоры"
      }
      _ <- msgServices.sendTextMessageWithKeyboard(chat, del, PidorButton.pidorButton(prv))
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

  def echoBack(chat: ChatApi, any: String)(implicit tgClient: TelegramClient[F]): F[Unit] = {
    for {
      _ <- msgServices.sendTextMessageOnly(chat, any)
    } yield ()
  }
}
