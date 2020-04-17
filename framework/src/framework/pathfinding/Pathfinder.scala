package framework.pathfinding

import framework.Square
import WaypointResult._

/**
  * A pathfinder.
  * @tparam ResultType The type of result this pathfinder should yield.
  * @version alpha 0.1
  * @author Felix Lehner
  */
abstract class Pathfinder[@specialized(Boolean) ResultType] {
  /** ''Sets'' this pathfinder on a square to start it from there. */
  def apply(pos: Square): Result[ResultType] = decision(pos) match {
    case Termination => terminate(pos)
    case Positive => success(pos)
    case Continuation => continue(pos)
  }

  /** Terminates the pathfinder */
  def terminate(on: Square): Result[ResultType]

  /** Terminates with a positive result. */
  def success(on: Square): Result[ResultType]

  /** This lets the pathfinder continue its way. */
  def continue(from: Square): Result[ResultType]

  /** Decides what the pathfinder should do. */
  def decision(pos: Square): WaypointResult.Value
}
