package chess.graphics

import chess.framework.GameStatus.GameResult
import chess.framework.{ChessBoard, ChessIO, SquareCoordinate}
import chess.graphics.BoardColors.BoardColor
import chess.graphics.BoardColors.Brown._

import scala.swing._

class Board extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  val promoMenu = new PromotionChooser(400, 100)
  var board: ChessBoard = ChessBoard.classicalBoard(this)
  listenTo(promoMenu)

  setup()

  def update(boardOpt: Option[ChessBoard]): Unit = {
    boardOpt match {
      case Some(b) => board = b
      case None =>
    }
    repaint()
  }

  override def repaint(): Unit = {
    reload()
    super.repaint()
  }

  def reload(): Unit =
    contents foreach {
      case sq: Square =>
        sq.piece = board(sq.pos)
      case _ =>
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

  def unselect(square: SquareCoordinate): Unit = {
    val row = 9 - square.row
    val col = square.colIndx + 1
    val indx = col + 9 * row - 10
    contents(indx) match {
      case s: Square => s.unselect()
      case _ =>
    }
  }

  def unselectAll(): Unit =
    contents foreach {
      case s: Square => s.unselect()
      case _ =>
    }

  override def showDrawOffer(): Unit = {
    Debugger debug s"draw?"
  }

  override def removeDrawOffer(): Unit = {
    Debugger debug s"draw decided"
  }

  override def showPromotion(): Unit = {
    promoMenu.open
    Debugger debug s"promote?"
  }

  override def removePromotion(): Unit = {
    promoMenu.close
    Debugger debug s"promoted!"
  }

  override def showTakeback(): Unit = {
    Debugger debug s"takeback?"
  }

  override def removeTakeback(): Unit = {
    Debugger debug s"takeback!"
  }

  override def showEnded(result: GameResult): Unit = {
    Debugger debug s"Ended with $result"
  }
}
