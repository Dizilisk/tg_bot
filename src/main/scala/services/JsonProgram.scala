package services

import cats._
import cats.implicits.toFlatMapOps
import repositories.JsonRepo
import repositories.rawmodel.JsonAnswer

class JsonProgram[F[_] : Monad](json: JsonService[F]) {

  def get = {
    ???
  }

}
