package chess.framework.pathfinding
import chess.framework.Square

abstract class MonoDirectionalPathfinder[@specialized(Boolean) ResultType](override val vector: (Int, Int)) extends VectorPathfinder[ResultType](vector) {
  override def continue(from: Square): Result[ResultType] = this.apply(from + vector)
}
