package graphics

import chess.graphics.BoardColors.BoardColor
import chess.graphics.{PieceButton, PromotionEvent}

import scala.swing._

class PromoOptionButton(val pieceColor: AnyColor, val pieceType: (AnyColor, Boolean) => AnyPiece, val backgroundCol: BoardColor)
  extends Button with PieceButton {

  override val eventType: PromoOptionButton.this.type => ActionEvent = PromotionEvent

  override val piece: Piece = pieceType(pieceColor, false)
  override val color: BoardColor = backgroundCol

  override def piece_= (p: Piece): Unit = ()
  override def color_= (c: BoardColor): Unit = ()
}
