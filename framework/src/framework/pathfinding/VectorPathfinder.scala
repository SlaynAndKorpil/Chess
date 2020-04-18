package framework.pathfinding

/**
  * A pathfinder that searches in a specified direction.
  * @param vector the direction
  * @version alpha 0.1
  * @author Felix Lehner
  */
abstract class VectorPathfinder[@specialized(Boolean) Result](val vector: (Int, Int)) extends Pathfinder[Result]
