package framework

/**
  * Stores a move.
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
sealed class MoveData(val startPos: Square, val piece: Piece, val endPos: Square, val captured: Boolean) {
  def xml: Elem = <move start={startPos.toString} end={endPos.toString} capture={captured.toString} piece={piece.toString}/>
}

object MoveData {
  def apply(startPos: Square, piece: Piece, endPos: Square, captured: Boolean): MoveData = new MoveData(startPos, piece, endPos, captured)

  def unapply(md: MoveData): Option[(Square, Piece, Square, Boolean)] = Some(md.startPos, md.piece, md.endPos, md.captured)
}

case class PromotionMove(override val startPos: Square,
                         override val piece: Piece,
                         override val endPos: Square,
                         override val captured: Boolean,
                         promoPiece: Piece) extends MoveData(startPos, piece, endPos, captured) {
  override def xml: Elem = <move start={startPos.toString} end={endPos.toString} capture={captured.toString} piece={piece.toString} promoPiece={promoPiece.toString}/>
}
