package chess.framework.pathfinding

abstract class VectorPathfinder[@specialized(Boolean) Result](val vector: (Int, Int)) extends Pathfinder[Result]
