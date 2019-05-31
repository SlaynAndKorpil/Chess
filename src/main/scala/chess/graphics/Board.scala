package chess.graphics

import chess.framework.GameStatus._
import chess.framework.{Black => _, White => _, _}
import chess.graphics.BoardColors.BoardColor
import chess.graphics.BoardColors.Brown._

import scala.swing._

class Board extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  val promoMenu = new PromotionChooser(400, 100)
  var board: ChessBoard = ChessBoard.classicalBoard(this)
  listenTo(promoMenu)

  implicit lazy private val promotionLoc: Component = contents.head

  setup()

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
    CDialog.showConfirmation(message = "Do you want a draw?", title = "Draw offer", onSuccess = () => {
      update(board.receive(DrawAcceptance))
    }, onRejection = () => update(board.receive(DrawReject)))
  }

  override def removeDrawOffer(): Unit = {}

  override def showPromotion(): Unit = {
    promoMenu.open()
  }

  override def removePromotion(): Unit = {
    promoMenu.close()
  }

  override def showTakeback(): Unit = {
    CDialog.showConfirmation(message = "Do you want to allow your opponent a takeback?", title = "Takeback", onSuccess = () => {
      update(board.receive(TakebackAcceptance))
    }, onRejection = () => update(board.receive(TakebackReject)))
  }

  def update(boardOpt: Option[(ChessBoard, () => Unit)]): Unit = {
    boardOpt match {
      case Some(b) =>
        board = b._1
        b._2()
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

  override def removeTakeback(): Unit = {}

  override def showEnded(result: GameResult): Unit = {
    val resultMessage: String = {
      val res = result match {
        case BlackWins(reason) => "win for black due to " + reason.toString
        case WhiteWins(reason) => "win for white due to " + reason.toString
        case Draw(reason) => "draw due to " + reason.toString
      }
      s"The game ended with a $res."
    }

    CDialog.showMessage(resultMessage, "")
  }
}
