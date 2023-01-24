package domain

case class UserInfo(userId: Long, username: Option[String], firstName: String, lastName: Option[String])
