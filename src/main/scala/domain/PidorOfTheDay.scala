package domain

sealed trait PidorOfTheDay
case class AlreadySelected(username: String) extends PidorOfTheDay
case class TodayPidor(username: String) extends PidorOfTheDay
