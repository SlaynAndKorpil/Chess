package chess.framework.pathfinding

import chess.framework.Square
import WaypointResult._

abstract class Pathfinder[@specialized(Boolean) ResultType] {
  def apply(pos: Square): Result[ResultType] = decision(pos) match {
    case Termination => terminate(pos)
    case Positive => success(pos)
    case Continuation => continue(pos)
  }

  def terminate(on: Square): Result[ResultType]

  def success(on: Square): Result[ResultType]

  def continue(from: Square): Result[ResultType]

  def decision(pos: Square): WaypointResult.Value
}
