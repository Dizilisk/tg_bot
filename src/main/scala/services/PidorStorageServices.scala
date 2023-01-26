package services

import cats.Monad
import cats.implicits.catsSyntaxApplicativeId
import repositories.{PidorRepo, rawmodel}
import cats.syntax.flatMap._
import cats.syntax.functor._
import domain.{TopPidors, UserInfo}
import repositories.rawmodel.PidorStat

import java.time.LocalDate


class PidorStorageServices[F[_] : Monad](pidorRepo: PidorRepo[F]) {

  def pidorReg(chat_id: Long, userInfo: UserInfo): F[Unit] = {
    pidorRepo.pidorStat.flatMap {
      userstat =>
        if (userstat.exists(user => user.chat_id == chat_id && user.user_id == userInfo.userId)) ().pure
        else pidorRepo.pidorReg(chat_id, userInfo).void
    }
  }

  def pidorStat(chat_id: Long): F[List[TopPidors]] = {
    for {
      stat <- pidorRepo.pidorStat
      top = stat.filter(_.chat_id == chat_id).sortBy(_.pidor_count).take(10).reverse.zipWithIndex.map{
        case (stat, i) => TopPidors(i + 1, stat.user_id, stat.username, stat.first_name, stat.pidor_count)
      }
    } yield top
  }

  def pidorSelfStat(chat_id: Long, id: Long): F[Option[PidorStat]] = {
    pidorRepo.pidorSelf(id, chat_id)
  }

  def pidorDel(chat_id: Long, id: Long): F[DeletionResult] = {
    pidorRepo.pidorStat.flatMap {
      clear =>
        if (clear.map(_.user_id).contains(id)) {
          pidorRepo.delFromPidor(chat_id, id).map(p => Successful)
        }
        else AlreadyDeleted.pure.widen
    }
  }

  def pidorGet(chat_id: Long, time: LocalDate): F[Option[rawmodel.TodayPidor]] = {
    val todayPidor = pidorRepo.currentPidor(chat_id, time)
    todayPidor
  }

  def updatePidorOfTheDay(chat_id: Long, user_id: Long, date: LocalDate): F[Unit] = {
    for {
      _ <- pidorRepo.incrementPidorCount(chat_id, user_id)
      _ <- pidorRepo.pidorDay(chat_id, user_id, date)
    } yield ()
  }

}
