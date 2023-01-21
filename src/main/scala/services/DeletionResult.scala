package services

sealed trait DeletionResult
case object Successful extends DeletionResult
case object AlreadyDeleted extends DeletionResult
