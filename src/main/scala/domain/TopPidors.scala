package domain

case class TopPidors(place: Int,
                     user_id: Long,
                     username: Option[String],
                     first_name: String,
                     pidor_count: Int)
