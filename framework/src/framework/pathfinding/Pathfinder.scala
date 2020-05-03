package framework.pathfinding

import framework.Sqr
import WaypointResult._

/**
  * A pathfinder.
  * @tparam ResultType The type of result this pathfinder should yield.
  * @version alpha 0.1
  * @author Felix Lehner
  */
abstract class Pathfinder[@specialized(Boolean) ResultType] {
  /** ''Sets'' this pathfinder on a square to start it from there. */
  def apply(pos: Sqr): Result[ResultType] = decision(pos) match {
    case Termination => terminate(pos)
    case Positive => success(pos)
    case Continuation => continue(pos)
  }

  /** Terminates the pathfinder */
  def terminate(on: Sqr): Result[ResultType]

  /** Terminates with a positive result. */
  def success(on: Sqr): Result[ResultType]

  /** This lets the pathfinder continue its way. */
  def continue(from: Sqr): Result[ResultType]

  /** Decides what the pathfinder should do. */
  def decision(pos: Sqr): WaypointResult.Value
}
