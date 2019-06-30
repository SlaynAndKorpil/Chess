package chess.framework.pathfinding

/**
  * Possible results of a pathfinder decision.
  * Depending on this result it will either continue, succeed or terminate.
  */
object WaypointResult extends Enumeration {
  val Termination = Value("Termination")
  val Positive = Value("Positive")
  val Continuation = Value("Continuation")
}
