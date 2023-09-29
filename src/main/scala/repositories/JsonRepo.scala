package repositories

import cats.effect.Sync
import doobie.implicits.toSqlInterpolator
import doobie.util.transactor.Transactor
import doobie.implicits._
import repositories.rawmodel.JsonAnswer
import routes.HttpResponse

class JsonRepo[F[_] : Sync](trans: Transactor[F]) {

  def insertMessage(message: String): F[Int] = {
    sql"""insert into baka_bot.greet_message
         (message)
          values ($message)
       """
      .update
      .run.transact(trans)
  }

  def updateMessage(message: String): F[Int] = {
    sql"""update baka_bot.greet_message set message = ($message) """
      .update
      .run.transact(trans)
  }

  def checkMessage: F[Option[JsonAnswer]] = {
    sql"""select message from baka_bot.greet_message"""
      .query[JsonAnswer]
      .option.transact(trans)
  }

  def getMessage: F[List[JsonAnswer]] = {
    sql"""select message from baka_bot.greet_message"""
      .query[JsonAnswer]
      .to[List].transact(trans)
  }

}
