package services

import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import repositories.RpsRepo
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{Draw, GameResult, LoseGame, TopPlayers, UserInfo, WinGame}
import repositories.rawmodel.RpsStat

import scala.util.Random

class RpsStorageServices[F[_] : Monad](rps: RpsRepo[F]) {

  def gameReg(userInfo: UserInfo): F[Unit] = {
    rps.stat.flatMap {
      userList =>
        if (userList.map(_.user_id).contains(userInfo.userId)) ().pure
        else {
          rps.reg(userInfo).void
        }
    }
  }

  def gameStatReg(chat_id: Long, userInfo: UserInfo): F[Unit] = {
    rps.stat.flatMap {
      userStat =>
        if (userStat.exists(a => a.chat_id == chat_id && a.user_id == userInfo.userId)) ().pure
        else rps.gamereg(chat_id, userInfo).void
    }
  }

  def gameStat(chat_id: Long): F[List[TopPlayers]] = {
    for {
      stat <- rps.stat
      top = stat.filter(_.chat_id == chat_id).sortBy(_.win_count).take(10).reverse.zipWithIndex.map {
        case (rpsstat, i) => TopPlayers(i + 1, rpsstat.username, rpsstat.first_name, rpsstat.win_count)
      }
    } yield top
  }

  def gameSelfStat(chat_id: Long, id: Long): F[Option[RpsStat]] = {
    rps.stat.flatMap {
      self =>
        self.find(a => a.user_id == id && a.chat_id == chat_id).pure
    }
  }

  def gameDel(chat_id: Long, id: Long): F[DeletionResult] = {
    rps.stat.flatMap {
      clear =>
        if (clear.map(_.user_id).contains(id)) {
          rps.delFromRPS(chat_id, id).map(d => Successful)
        }
        else AlreadyDeleted.pure.widen
    }
  }

  def rpsGame(chat_id: Long, id: Long, player: String): F[GameResult] = {
    val r = "Камень"
    val p = "Бумага"
    val s = "Ножницы"
    val bot: List[String] = List(r, p, s)
    val botGet: String = bot(Random.nextInt(bot.length))
    player match {
      case _ if (player == r && botGet == s) ||
        (player == p && botGet == r) ||
        (player == s && botGet == p) =>
        val wins = rps.winCount(chat_id, id)
        for {
          wcc <- wins
          count = wcc + 1
          _ <- rps.updateWinCounter(chat_id, id, count)
        } yield WinGame(botGet, player)


      case _ if (player == s && botGet == r) ||
        (player == r && botGet == p) ||
        (player == p && botGet == s) =>
        val loses = rps.loseCount(chat_id, id)
        for {
          lcc <- loses
          count = lcc + 1
          _ <- rps.updateLoseCounter(chat_id, id, count)
        } yield LoseGame(botGet, player)


      case _ if player == botGet => Draw.pure.widen
    }
  }

  def gameClear(): F[Unit] = rps.delRpsDB.void

}
