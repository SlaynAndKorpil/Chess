package chess.framework

case class MoveData(startPos: SquareCoordinate, piece: Piece, endPos: SquareCoordinate, captured: Boolean)
