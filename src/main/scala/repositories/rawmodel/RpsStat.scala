package repositories.rawmodel

case class RpsStat(user_id: Long,
                   username: Option[String],
                   first_name: String,
                   last_name: Option[String],
                   chat_id: Long,
                   win_count: Int,
                   lose_count: Int)
