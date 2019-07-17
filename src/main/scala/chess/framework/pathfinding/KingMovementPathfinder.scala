package chess.framework.pathfinding

import chess.framework.Square

/** Applies the movement of a king to a pathfinder. */
abstract class KingMovementPathfinder extends Pathfinder[Boolean] {
  override def success(on: Square): Success[Boolean] = Success(true)

  override def continue(from: Square): Result[Boolean] = apply(from)

  override def apply(pos: Square): Result[Boolean] = {
    val agents = Array(
      KingMovement(0, 1), KingMovement(0, -1),
      KingMovement(1, 1), KingMovement(1, 0), KingMovement(1, -1),
      KingMovement(-1, 1), KingMovement(-1, 0), KingMovement(-1, -1),
    )

    val results: Array[Result[Boolean]] =
      agents
        .map(pathfinder => TerminableOnce[Boolean](pathfinder))
        .map(terminableOnce => terminableOnce.apply(pos + terminableOnce.pathfinder.vector))

    results find (_.isSuccess) match {
      case Some(success) => success
      case None => Failure
    }
  }

  override def terminate(on: Square): Failure.type = Failure

  override def decision(pos: Square): WaypointResult.Value

  private case class KingMovement(override val vector: (Int, Int))
    extends TripleDirectionalPathfinder[Boolean](vector) with BooleanPathfinder {
    override def decision(pos: Square): WaypointResult.Value = KingMovementPathfinder.this.decision(pos)
  }
}
