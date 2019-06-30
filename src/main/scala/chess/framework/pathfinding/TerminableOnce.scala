package chess.framework.pathfinding
import chess.framework.Square

/**
  * Wraps another pathfinder and lets it run in a sandbox-like
  * manner so it gets 'reflected' the first time it is terminated.
  * @version alpha 0.1
  * @author Felix Lehner
  */
case class TerminableOnce[@specialized(Boolean) ResultType](pathfinder: VectorPathfinder[ResultType]) extends Pathfinder[ResultType] {
  val pF = new TripleDirectionalPathfinder[ResultType](pathfinder.vector) {
    override def success(on: Square): Result[ResultType] = pathfinder.success(on)

    override def decision(pos: Square): WaypointResult.Value = pathfinder.decision(pos)

    override def terminate(on: Square): Result[ResultType] = TerminableOnce.this.terminate(on)
  }

  override def terminate(on: Square): Result[ResultType] = {
    val vectors = Array(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    ).filterNot(_ == pathfinder.vector)

    val results = for {
      vector <- vectors
      startPos = on - pathfinder.vector
    } yield TerminatedPathfinder(vector)(startPos)

    results find (_.isSuccess) match {
      case Some(success) => success
      case None => pathfinder.terminate(on)
    }
  }

  private case class TerminatedPathfinder(override val vector: (Int, Int)) extends TripleDirectionalPathfinder[ResultType](vector) {
    override def terminate(on: Square): Result[ResultType] = pathfinder.terminate(on)

    override def success(on: Square): Result[ResultType] = pathfinder.success(on)

    override def decision(pos: Square): WaypointResult.Value = pathfinder.decision(pos)
  }

  override def success(on: Square): Result[ResultType] = pathfinder.success(on)

  override def continue(from: Square): Result[ResultType] = pF.continue(from)

  override def decision(pos: Square): WaypointResult.Value = pathfinder.decision(pos)
}
