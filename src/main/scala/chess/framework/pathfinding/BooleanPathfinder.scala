package chess.framework.pathfinding
import chess.framework.Square

/** A pathfinder that returns [[scala.Boolean]] results. */
trait BooleanPathfinder extends Pathfinder[Boolean] {
  override def terminate(on: Square): Failure.type = Failure

  override def success(on: Square): Success[Boolean] = Success(true)
}
