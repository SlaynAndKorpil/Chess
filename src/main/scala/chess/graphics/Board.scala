package chess.graphics

import chess.framework.BoardStatus.GameResult._
import chess.framework.BoardStatus.ResultReason.DrawResultReason.InsufficientMaterial
import chess.framework.IOEvents._
import chess.framework.Input._
import chess.framework.FileOperationError.FileOperationError
import chess.framework._
import chess.graphics.BoardColors._

import scala.swing._

class Board extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  //TODO maybe use a real popup for this?
  val promoMenu = new PromotionChooser(400, 100)
  listenTo(promoMenu)

  override protected var board: ChessBoard = ChessBoard.classicalBoard

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
          val pos = Square(col, row)
          new SquareButton(color, pos, chessBoard(pos))
        }
    }
    contents foreach (comp => listenTo(comp))
  }

  def unselect(square: Square): Unit =
    getSquareOnCoordinate(square) match {
      case Some(s) => s.unselect()
      case _ =>
    }

  private def getSquareOnCoordinate(square: Square): Option[SquareButton] =
    if (square.isValid) {
      val row = 9 - square.row
      val col = square.colIndx + 1
      val indx = col + 9 * row - 10
      contents(indx) match {
        case s: SquareButton => Some(s)
        case _ => None
      }
    }
    else None

  def unselectAll(): Unit =
    contents foreach {
      case s: SquareButton => s.unselect()
      case _ =>
    }

  override def update(): Unit = repaint()

  chessReactions += {
    case ShowEnded(result) =>
      CDialog.showMessage({
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
      }, "")

    case ShowTakeback =>
      CDialog.showConfirmation(message = "Do you want to allow your opponent a takeback?", title = "Takeback", onSuccess = () => {
        receiveInput(TakebackAcceptance)
      }, onRejection = () => receiveInput(TakebackReject))

    case RemovePromotion =>
      promoMenu.close()

    case ShowPromotion(pos) =>
      getSquareOnCoordinate(pos) match {
        case Some(square) =>
          square.piece.color match {
            case color: AnyColor =>
              promoMenu.open(color, square.locationOnScreen)
            case _ =>
          }
        case None =>
      }

    case ShowDrawOffer =>
      CDialog.showConfirmation(message = "Do you want a draw?", title = "Draw offer", onSuccess = () => {
        receiveInput(DrawAcceptance)
      }, onRejection = () => receiveInput(DrawReject))

    case ShowCheck(pos) =>
      displayCheck(pos)
  }

  def displayCheck(on: Square): Unit =
    getSquareOnCoordinate(on) match {
      case Some(square) =>
        square.checked = true
        square.repaint()
      case None =>
    }

  override def repaint(): Unit = {
    reload()
    super.repaint()
  }

  def reload(): Unit =
    contents foreach {
      case sq: SquareButton =>
        sq.checked = false
        sq.piece = chessBoard(sq.pos)
      case _ =>
    }

  override def receiveInput(input: Input[_]): Unit = super.receiveInput(input)

  override def load(filePath: String): Option[FileOperationError] = super.load(filePath)
}
