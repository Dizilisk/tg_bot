package services

import cats.Monad
import cats.implicits.{catsSyntaxApplicativeId, toFlatMapOps}
import repositories.JsonRepo
import repositories.rawmodel.JsonAnswer
import routes.HttpResponse

class JsonService[F[_] : Monad](jsonRepo: JsonRepo[F]) {

  def messageReg(message: HttpResponse): F[Int] = jsonRepo.checkMessage.flatMap {
    case Some(value) => jsonRepo.updateMessage(message.message)
    case None => jsonRepo.insertMessage(message.message)
  }

  def getMessage: F[List[JsonAnswer]] = jsonRepo.checkMessage.flatMap {
    case Some(value) => jsonRepo.getMessage
  }

}
