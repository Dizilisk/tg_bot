package repositories.rawmodel

case class PidorStat(user_id: Long,
                     username: Option[String],
                     first_name: String,
                     last_name: Option[String],
                     chat_id: Long,
                     pidor_count: Int)