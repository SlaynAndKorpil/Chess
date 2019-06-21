package chess.framework.pathfinding
import chess.framework.Square

case class TerminableOnce[ResultType](pathfinder: VectorPathfinder[ResultType]) extends Pathfinder[ResultType] {
  override def terminate(on: Square): Result[ResultType] = {
    val vectors = Array(
      (-1, -1), (-1, 0), (-1, 1),
      (0, -1), (0, 1),
      (1, -1), (1, 0), (1, 1)
    ).filterNot(_ == pathfinder.vector)

    val results = for {
      vector <- vectors
      startPos = on + vector
    } yield TerminatedPathfinder(vector)(startPos)

    if (results contains success(on)) success(on)
    else pathfinder.terminate(on)
  }

  private case class TerminatedPathfinder(override val vector: (Int, Int)) extends TripleDirectionalPathfinder[ResultType](vector) {
    override def terminate(on: Square): Result[ResultType] = pathfinder.terminate(on)

    override def success(on: Square): Result[ResultType] = pathfinder.success(on)

    override def decision(pos: Square): WaypointResult.Value = pathfinder.decision(pos)
  }

  override def success(on: Square): Result[ResultType] = pathfinder.success(on)

  override def continue(from: Square): Result[ResultType] = pathfinder.continue(from)

  override def decision(pos: Square): WaypointResult.Value = pathfinder.decision(pos)
}
