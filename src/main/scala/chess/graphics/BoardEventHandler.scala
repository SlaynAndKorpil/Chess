package chess.graphics

import framework.Input._
import framework._

trait BoardEventHandler {
  self: Board =>

  private var selectedSquare: Square = _

  reactions += {
    case SquarePressed(square) =>
      val clickedPos = square.pos
      val selectedColor = square.piece.color
      if (selectedSquare == null)
        if (board.turn == selectedColor) selectedSquare = clickedPos
        else unselect(clickedPos)
      else if (clickedPos == selectedSquare) {
        unselect(clickedPos)
        selectedSquare = null
      }
      else if (selectedColor == board.turn) {
        unselect(selectedSquare)
        selectedSquare = clickedPos
      }
      else {
        unselect(clickedPos)
        unselect(selectedSquare)
        move(from = selectedSquare, to = clickedPos)
        selectedSquare = null
      }
    case PromotionEvent(piece) =>
      promote(piece.pieceType)
  }

  def saveBoard(file: String): Unit = ChessBoard.save(board, file)

  private def move(from: Square, to: Square): Unit =
    receiveInput(MoveParams(from, to))

  private def promote(piece: (AnyColor, Boolean) => AnyPiece): Unit =
    receiveInput(Promotion(piece))

}
