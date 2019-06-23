package chess.framework.pathfinding

sealed abstract class Result[@specialized(Boolean) +T] {
  def content: T

  def isSuccess: Boolean
}

case class Success[@specialized(Boolean) +T](override val content: T) extends Result[T] {
  override def isSuccess: Boolean = true
}

object Failure extends Result[Nothing] {
  override def content: Nothing = throw new NoSuchElementException("Failure.content")

  override def isSuccess: Boolean = false
}
