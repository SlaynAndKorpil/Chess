package framework

/**
  * Stores a move.
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
sealed class MoveData(val startPos: Sqr, val piece: Piece, val endPos: Sqr, val captured: Boolean) {
  def xml: scala.xml.Elem =
      <move start={startPos.toString} end={endPos.toString} capture={captured.toString} piece={piece.toString}/>
}

object MoveData {
  def apply(startPos: Sqr, piece: Piece, endPos: Sqr, captured: Boolean): MoveData =
    new MoveData(startPos, piece, endPos, captured)

  def unapply(md: MoveData): Option[(Sqr, Piece, Sqr, Boolean)] = Some(md.startPos, md.piece, md.endPos, md.captured)
}

case class PromotionMove(override val startPos: Sqr,
                         override val piece: Piece,
                         override val endPos: Sqr,
                         override val captured: Boolean,
                         promoPiece: Piece) extends MoveData(startPos, piece, endPos, captured) {
  override def xml: scala.xml.Elem =
      <move start={startPos.toString} end={endPos.toString} capture={captured.toString} piece={piece.toString} promoPiece={promoPiece.toString}/>
}
