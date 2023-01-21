package repositories

import cats.effect.Sync
import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import cats.syntax.functor._
import repositories.rawmodel.{Allstat, Selfstat}

class RpsRepo[F[_] : Sync](trans: Transactor[F]) {

  def reg(name: String, id: Long): F[Int] = {
    sql"insert into testshema.rps_leaderboard (player_name, player_id) values ($name, $id)"
      .update
      .run.transact(trans)
  }

  def stat: F[List[Allstat]] = {
    sql"select player_name, player_id, win_counter, lose_counter from testshema.rps_leaderboard order by win_counter desc"
      .query[Allstat]
      .to[List].transact(trans)
  }

  def self(id: Long): F[Option[Selfstat]] = {
    sql"select * from testshema.rps_leaderboard where player_id = $id"
      .query[Selfstat].option
      .transact(trans)
  }

  def delFromRPS(id: Long): F[Int] = {
    sql"delete from testshema.rps_leaderboard where player_id = $id"
      .update
      .run.transact(trans)
  }

  def winCount(id: Long): F[Int] = {
    sql"select win_counter from testshema.rps_leaderboard where player_id = $id"
      .query[Int]
      .to[List].transact(trans)
      .map(_.headOption.getOrElse(0))
  }

  def loseCount(id: Long): F[Int] = {
    sql"select lose_counter from testshema.rps_leaderboard where player_id = $id"
      .query[Int]
      .to[List].transact(trans)
      .map(_.headOption.getOrElse(0))
  }

  def updateWinCounter(id: Long, count: Int): F[Int] = {
    sql"update testshema.rps_leaderboard set win_counter = $count where player_id = $id"
      .update
      .run.transact(trans)
  }

  def updateLoseCounter(id: Long, count: Int): F[Int] = {
    sql"update testshema.rps_leaderboard set lose_counter = $count where player_id = $id"
      .update
      .run.transact(trans)
  }

  def delRpsDB: F[Int] = {
    sql"delete from testshema.rps_leaderboard"
      .update
      .run.transact(trans)
  }
}
