package framework.pathfinding
import framework.Sqr

abstract class MonoDirectionalPathfinder[@specialized(Boolean) ResultType](override val vector: (Int, Int))
  extends VectorPathfinder[ResultType](vector) {

  override def continue(from: Sqr): Result[ResultType] = this.apply(from + vector)
}
