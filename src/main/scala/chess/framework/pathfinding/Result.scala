package chess.framework.pathfinding

/** Result of a pathfinder. */
sealed trait Result[@specialized(Boolean) +T] {
  def content: T

  def isSuccess: Boolean
}

/** A positive result. */
case class Success[@specialized(Boolean) +T](override val content: T) extends Result[T] {
  override def isSuccess: Boolean = true
}

/** A failure. */
object Failure extends Result[Nothing] {
  override def content: Nothing = throw new NoSuchElementException("Failure.content")

  override def isSuccess: Boolean = false
}
