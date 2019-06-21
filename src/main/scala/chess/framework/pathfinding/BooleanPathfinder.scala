package chess.framework.pathfinding
import chess.framework.Square

trait BooleanPathfinder extends Pathfinder[Boolean] {
  override def terminate(on: Square): Failure.type = Failure

  override def success(on: Square): Success[Boolean] = Success(true)
}
