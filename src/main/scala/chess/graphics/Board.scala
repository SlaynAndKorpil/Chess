package chess.graphics

import chess.framework.BoardStatus.GameResult._
import chess.framework.BoardStatus.ResultReason._
import chess.framework.IOEvents._
import chess.framework.Input._
import chess.framework._
import chess.graphics.BoardColors._

import scala.swing._

class Board extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  val promoMenu = new PromotionChooser(400, 100)
  private[chess] var board: ChessBoard = ChessBoard.classicalBoard(this)
  listenTo(promoMenu)

  setup()

  def setup(): Unit = {
    contents ++= {
      for (i <- 1 to 9; j <- 0 to 8;
           row = 9 - i; col = chess.framework.ChessBoard.columnLetter(j)) yield
        if (i == 9 && j == 0) new CTextField()
        else if (i == 9) new CTextField(col.toString)
        else if (j == 0) new CTextField(row.toString)
        else {
          val color: BoardColor = if (i % 2 == j % 2) Brown.White else Brown.Black
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

  override def update(): Unit = repaint()

  override def react(event: IOEvent): Unit = event match {
    case ShowCheck(on) => println(s"Checked king on $on")

    case ShowEnded(result) =>
      val resultMessage: String = {
        val res = result match {
          case BlackWins(_) => "win for black"
          case WhiteWins(_) => "win for white"
          case Draw(_) => "draw"
        }
        val reason = result.reason match {
          case InsufficientMaterial => "insufficient material"
          case x => x.toString
        }
        s"The game ended with a $res due to $reason."
      }

      CDialog.showMessage(resultMessage, "")

    case ShowTakeback =>
      CDialog.showConfirmation(message = "Do you want to allow your opponent a takeback?", title = "Takeback", onSuccess = () => {
        receiveInput(TakebackAcceptance)
      }, onRejection = () => receiveInput(TakebackReject))

    case RemovePromotion => promoMenu.close()

    case ShowPromotion => promoMenu.open()

    case ShowDrawOffer =>
      CDialog.showConfirmation(message = "Do you want a draw?", title = "Draw offer", onSuccess = () => {
        receiveInput(DrawAcceptance)
      }, onRejection = () => receiveInput(DrawReject))

    case _ =>
  }

  override def receiveInput(input: Input[_]): Unit = {
    super.receiveInput(input)
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
}
