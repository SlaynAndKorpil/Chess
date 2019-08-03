package chess.framework

/**
  * Stores a move.
  * @author Felix Lehner
  * @version alpha 0.2
  */
case class MoveData(startPos: Square, piece: Piece, endPos: Square, captured: Boolean)
