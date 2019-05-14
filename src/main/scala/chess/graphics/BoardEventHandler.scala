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
//        TODO fix deadlock when moving (callback to io by framework.ChessBoard)!!
        move(from = selectedSquare, to = clickedPos)
        selectedSquare = null
      }

//    case scala.swing.event.
//    case MouseClicked(_, _, _, _, _) =>
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
