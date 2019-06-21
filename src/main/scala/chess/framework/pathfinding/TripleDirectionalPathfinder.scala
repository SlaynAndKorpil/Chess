package chess.framework.pathfinding
import chess.framework.Square

abstract class TripleDirectionalPathfinder[ResultType](override val vector: (Int, Int)) extends VectorPathfinder[ResultType](vector) {
  override def continue(from: Square): Result[ResultType] = {
    val res1 = new MonoDirectionalPathfinder[ResultType](0, vector._2) {
      override def terminate(on: Square): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

      override def success(on: Square): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

      override def decision(pos: Square): WaypointResult.Value = TripleDirectionalPathfinder.this.decision(pos)
    }.apply(from + vector)

    val res2 = this.apply(from + vector)

    val res3 = new MonoDirectionalPathfinder[ResultType](vector._1, 0) {
      override def terminate(on: Square): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

      override def success(on: Square): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

      override def decision(pos: Square): WaypointResult.Value = TripleDirectionalPathfinder.this.decision(pos)
    } apply(from + vector)

    Array(res1, res2, res3) find (_.isSuccess) match {
      case Some(success) => success
      case None => Failure
    }
  }
}
