package chess.graphics

import chess.framework.{ChessBoard, ChessIO, SquareCoordinate}
import chess.graphics.BoardColors.BoardColor
import chess.graphics.BoardColors.Brown._

import scala.swing._

class Board extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  var board: ChessBoard = ChessBoard.classicalBoard(this)

  setup()

  def update(boardOpt: Option[ChessBoard]): Unit = boardOpt match {
    case Some(b) => board = b
    case None =>
  }

  def setup(): Unit = {
    contents ++= {
      for (i <- 1 to 9; j <- 0 to 8;
           row = 9 - i; col = chess.framework.ChessBoard.columnLetter(j)) yield
        if (i == 9 && j == 0) new CTextField()
        else if (i == 9) new CTextField(col.toString)
        else if (j == 0) new CTextField(row.toString)
        else {
          val color: BoardColor = if (i % 2 == j % 2) Black else White
          val pos = SquareCoordinate(col, row)
          new Square(color, pos, board(pos))
        }
    }
    contents foreach (comp => listenTo(comp))
  }

  override def repaint(): Unit = {
    contents foreach {
      case sq: Square =>
        Debugger debug s"${sq.pos}: ${board(sq.pos)}"
        sq.piece = board(sq.pos)
      case _ =>
    }
    super.repaint()
  }

  def unselect(square: SquareCoordinate): Unit = {
    val row = 9 - square.row
    val col = square.colIndx + 1
    val indx = col + 9 * row - 10
    contents(indx) match {
      case s: Square => s.unselect()
      case _ =>
    }
  }

  override def showDrawOffer(): Unit = {
    Debugger debug s"draw?"
  }

  override def removeDrawOffer(): Unit = {
    Debugger debug s"draw decided"
  }

  override def showPromotion(): Unit = {
    Debugger debug s"promote!"
  }

  override def removePromotion(): Unit = {
    Debugger debug s"promoted!"
    repaint()
  }

  override def showResign(): Unit = {
    Debugger debug s"resign!"
  }
}
