package framework.pathfinding
import framework.Sqr

/** A pathfinder that returns [[scala.Boolean]] results. */
trait BooleanPathfinder extends Pathfinder[Boolean] {
  override def terminate(on: Sqr): Failure.type = Failure

  override def success(on: Sqr): Success[Boolean] = Success(true)
}
