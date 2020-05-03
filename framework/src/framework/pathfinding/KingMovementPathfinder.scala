package framework.pathfinding

import framework.Sqr

/**
  * Applies the movement of a king to a pathfinder.
  * @author Felix Lehner
  * @version alpha 0.2
  */
abstract class KingMovementPathfinder extends Pathfinder[Boolean] {
  override def success(on: Sqr): Success[Boolean] = Success(true)

  override def continue(from: Sqr): Result[Boolean] = apply(from)

  override def apply(pos: Sqr): Result[Boolean] = {
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

  override def terminate(on: Sqr): Failure.type = Failure

  override def decision(pos: Sqr): WaypointResult.Value

  private case class KingMovement(override val vector: (Int, Int))
    extends TripleDirectionalPathfinder[Boolean](vector) with BooleanPathfinder {
    override def decision(pos: Sqr): WaypointResult.Value = KingMovementPathfinder.this.decision(pos)
  }
}
