package repositories

import cats.effect.Sync
import cats.implicits.toFunctorOps
import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import domain.{TopPidors, UserInfo}
import repositories.rawmodel.{PidorStat, TodayPidor}

import java.time.LocalDate

class PidorRepo[F[_] : Sync](trans: Transactor[F]) {

  def pidorReg(chat: Long, userInfo: UserInfo): F[Int] = {
    sql"""insert into baka_bot.pidor_stat
         (chat_id, user_id)
          values ($chat, ${userInfo.userId})
       """
      .update
      .run.transact(trans)
  }

  def pidorStat: F[List[PidorStat]] = {
    sql"""select
          us.user_id,
          us.user_name,
          us.first_name,
          us.last_name,
          ps.chat_id,
          ps.pidor_count
          from baka_bot.users us join baka_bot.pidor_stat ps on ps.user_id = us.user_id
          order by pidor_count desc"""
      .query[PidorStat]
      .to[List].transact(trans)
  }

  def pidorSelf(user_id: Long, chat_id: Long): F[Option[PidorStat]] = {
    sql"""select
          us.user_id,
          us.user_name,
          us.first_name,
          us.last_name,
          ps.chat_id,
          ps.pidor_count
          from baka_bot.users us join baka_bot.pidor_stat ps on ps.user_id = us.user_id
          where us.user_id = $user_id and chat_id = $chat_id
       """
      .query[PidorStat]
      .option.transact(trans)
  }

  def delFromPidor(chat_id: Long, id: Long): F[Int] = {
    sql"delete from baka_bot.pidor_stat where chat_id = $chat_id and user_id = $id"
      .update
      .run.transact(trans)
  }

  def getPidorCount(chat_id: Long, id: Long): F[Int] = {
    sql"select pidor_count from baka_bot.pidor_stat where user_id = $id and chat_id = $chat_id"
      .query[Int]
      .to[List].transact(trans)
      .map(_.headOption.getOrElse(0))
  }

  def incrementPidorCount(chat_id: Long, id: Long): F[Int] = {
    sql"update baka_bot.pidor_stat set pidor_count = pidor_count + 1 where user_id = $id and chat_id = $chat_id"
      .update
      .run.transact(trans)
  }

  def currentPidor(chat_id: Long, date: LocalDate): F[Option[TodayPidor]] = {
    sql"select date, chat_id, user_id from baka_bot.pidor_winners_new where chat_id = $chat_id and date = ${date.toString}"
      .query[TodayPidor]
      .option.transact(trans)
  }

  def pidorDay(chat_id: Long, id: Long, time: LocalDate): F[Int] = {
    sql"insert into baka_bot.pidor_winners_new (date, user_id, chat_id) values (${time.toString}, $id, $chat_id)"
      .update
      .run.transact(trans)
  }
}
