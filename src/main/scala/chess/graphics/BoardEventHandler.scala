package chess.graphics

import chess.framework._

trait BoardEventHandler {
  self: Board =>

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
      promoMenu.close()
      update(board.receive(Promotion(piece.pieceType)))
  }

  def move(from: SquareCoordinate, to: SquareCoordinate): Unit = {
    board.receive(MoveParams(from, to)) match {
      case Some(b) =>
        board = b
        repaint()
      case None =>
    }
  }

  def saveBoard(file: String): Unit = ChessBoard.save(board, file)

  var selectedSquare: SquareCoordinate = _
}
