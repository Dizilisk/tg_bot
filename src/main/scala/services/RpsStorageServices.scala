package services


import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import repositories.RpsRepo
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{Draw, GameResult, LoseGame, TopPlayers, WinGame}
import repositories.rawmodel.Allstat

import scala.util.Random

class RpsStorageServices[F[_] : Monad](rps: RpsRepo[F]) {

  def gameReg(name: String, id: Long): F[Unit] = {
    rps.stat.flatMap {
      userList =>
        if (userList.map(_.player_id).contains(id)) ().pure
        else rps.reg(name, id).void
    }
  }

  def gameStat: F[List[TopPlayers]] = {
    for {
      stat <- rps.stat
      top = stat.sortBy(_.win_counter).take(10).reverse.zipWithIndex.map {
        case (allstat, i) => TopPlayers(i + 1, allstat.player_name, allstat.win_counter)
      }
    } yield top
  }

  def gameSelfStat(id: Long): F[Option[Allstat]] = {
    rps.stat.flatMap {
      self =>
        self.find(_.player_id == id).pure
    }
  }

  def gameDel(id: Long): F[DeletionResult] = {
    rps.stat.flatMap {
      clear =>
        if (clear.map(_.player_id).contains(id)) {
          rps.delFromRPS(id).map(d => Successful)
        }
        else AlreadyDeleted.pure.widen
    }
  }

  def rpsGame(id: Long, player: String): F[GameResult] = {
    val r = "Камень"
    val p = "Бумага"
    val s = "Ножницы"
    val bot: List[String] = List(r, p, s)
    val botGet: String = bot(Random.nextInt(bot.length))
    player match {
      case _ if (player == r && botGet == s) ||
        (player == p && botGet == r) ||
        (player == s && botGet == p) =>
        val wins = rps.winCount(id)
        for {
          wcc <- wins
          count = wcc + 1
          _ <- rps.updateWinCounter(id, count)
        } yield WinGame(botGet, player)


      case _ if (player == s && botGet == r) ||
        (player == r && botGet == p) ||
        (player == p && botGet == s) =>
        val loses = rps.loseCount(id)
        for {
          lcc <- loses
          count = lcc + 1
          _ <- rps.updateLoseCounter(id, count)
        } yield LoseGame(botGet, player)


      case _ if player == botGet => Draw.pure.widen
    }
  }

  def gameClear(): F[Unit] = rps.delRpsDB.void

}
