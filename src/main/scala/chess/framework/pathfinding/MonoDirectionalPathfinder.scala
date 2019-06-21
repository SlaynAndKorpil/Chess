package chess.framework.pathfinding
import chess.framework.Square

abstract class MonoDirectionalPathfinder[ResultType](override val vector: (Int, Int)) extends VectorPathfinder[ResultType](vector) {
  override def continue(from: Square): Result[ResultType] = this(from + vector)
}
