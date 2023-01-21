package domain

sealed trait GameResult
case class WinGame(bot: String, player: String) extends GameResult
case class LoseGame(bot: String, player: String) extends GameResult
case object Draw extends GameResult