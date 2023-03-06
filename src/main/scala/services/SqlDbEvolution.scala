package services

import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult

import scala.concurrent.{ExecutionContext, Future}

class SqlDbEvolution(flyway: Flyway) {

  /**
   * @return Migrations count
   */
  def runEvolutions()(implicit ec: ExecutionContext): Future[MigrateResult] = Future(flyway.migrate())

}

object SqlDbEvolution {

  def apply(): SqlDbEvolution = {
    lazy val flyway: Flyway = Flyway
      .configure()
      .dataSource("jdbc:", "", "")
      .locations("flyway")
      .load()

    new SqlDbEvolution(flyway)
  }
}

