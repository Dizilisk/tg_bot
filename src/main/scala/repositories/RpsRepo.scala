package repositories

import cats.effect.Sync
import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import cats.syntax.functor._
import domain.UserInfo
import repositories.rawmodel.RpsStat

class RpsRepo[F[_] : Sync](trans: Transactor[F]) {

  def reg(userInfo: UserInfo): F[Int] = {
    sql"""insert into baka_bot.users
          (user_id, user_name, first_name, last_name)
           values
           (${userInfo.userId}, ${userInfo.username}, ${userInfo.firstName}, ${userInfo.lastName})
           on conflict (user_id) do nothing
           """
      .update
      .run.transact(trans)
  }

  def gamereg(chat: Long, userInfo: UserInfo): F[Int] = {
    sql"""insert into baka_bot.game_stat
          (chat_id, user_id)
         values ($chat, ${userInfo.userId})
       """
      .update
      .run.transact(trans)
  }

  def stat: F[List[RpsStat]] = {
    sql"""select
          us.user_id,
          us.user_name,
          us.first_name,
          us.last_name,
          gs.chat_id,
          gs.win_count,
          gs.lose_count
          from baka_bot.users us join baka_bot.game_stat gs on gs.user_id = us.user_id
          order by win_count desc"""
      .query[RpsStat]
      .to[List].transact(trans)
  }

  def delFromRPS(chat_id: Long, id: Long): F[Int] = {
    sql"delete from baka_bot.game_stat where user_id = $id and chat_id = $chat_id"
      .update
      .run.transact(trans)
  }

  def winCount(chat_id: Long, id: Long): F[Int] = {
    sql"select win_count from baka_bot.game_stat where user_id = $id and chat_id = $chat_id"
      .query[Int]
      .to[List].transact(trans)
      .map(_.headOption.getOrElse(0))
  }

  def loseCount(chat_id: Long, id: Long): F[Int] = {
    sql"select lose_count from baka_bot.game_stat where user_id = $id and chat_id = $chat_id"
      .query[Int]
      .to[List].transact(trans)
      .map(_.headOption.getOrElse(0))
  }

  def updateWinCounter(chat_id: Long, id: Long, count: Int): F[Int] = {
    sql"update baka_bot.game_stat set win_count = $count where user_id = $id and chat_id = $chat_id"
      .update
      .run.transact(trans)
  }

  def updateLoseCounter(chat_id: Long, id: Long, count: Int): F[Int] = {
    sql"update baka_bot.game_stat set lose_count = $count where user_id = $id and chat_id = $chat_id"
      .update
      .run.transact(trans)
  }

  def delRpsDB: F[Int] = {
    sql"delete from baka_bot.game_stat"
      .update
      .run.transact(trans)
  }
}
