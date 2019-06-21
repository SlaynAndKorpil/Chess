package chess.framework.pathfinding

object WaypointResult extends Enumeration {
  val Termination = Value("Termination")
  val Positive = Value("Positive")
  val Continuation = Value("Continuation")
}
