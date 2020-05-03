package framework.pathfinding

import framework.Sqr

abstract class TripleDirectionalPathfinder[@specialized(Boolean) ResultType](override val vector: (Int, Int))
  extends VectorPathfinder[ResultType](vector) {

  override def continue(from: Sqr): Result[ResultType] = {
    val resMain = this.apply(from + vector)

    val sideVectors: Array[(Int, Int)] =
      if (vector._1 == 0) Array((1, 0), (-1, 0))
      else if (vector._2 == 0) Array((0, 1), (0, -1))
      else Array((0, vector._2), (vector._1, 0))

    val resSide = sideVectors map { v =>
      new MonoDirectionalPathfinder[ResultType](v) {
        override def terminate(on: Sqr): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

        override def success(on: Sqr): Result[ResultType] = TripleDirectionalPathfinder.this.terminate(on)

        override def decision(pos: Sqr): WaypointResult.Value = TripleDirectionalPathfinder.this.decision(pos)
      } apply (from + v)
    }

    resSide :+ resMain find (_.isSuccess) match {
      case Some(success) => success
      case None => terminate(from)
    }
  }
}
