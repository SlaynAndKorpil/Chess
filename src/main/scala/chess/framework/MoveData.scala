package chess.framework

case class MoveData(startPos: Square, piece: Piece, endPos: Square, captured: Boolean)
