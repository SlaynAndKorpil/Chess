package graphics

import framework.BoardStatus.GameResult._
import framework.BoardStatus.ResultReason.DrawResultReason.InsufficientMaterial
import framework.IOEvents._
import framework.{ChessBoard, ChessIO, Sqr, AnyColor}
import framework.FileOperationError._
import framework.Input._

import BoardColors._

import scala.swing._

class Board(val window: CWindow, loadGameFrom: String) extends GridPanel(0, 9) with BoardEventHandler with ChessIO {
  //TODO maybe use a real popup for this?
  val promoMenu = new PromotionChooser(400, 100)
  listenTo(promoMenu)

  override protected var board: ChessBoard =
    if (loadGameFrom.isEmpty) ChessBoard.classicalBoard
    else {
      ChessBoard.load(loadGameFrom) match {
        case Left(error) =>
          Error write error.description
          ChessBoard.classicalBoard
        case Right(board) => board
      }
    }

  setup()

  def setup(): Unit = {
    contents ++= {
      for (i <- 1 to 9; j <- 0 to 8;
           row = 9 - i; col = framework.ChessBoard.columnLetter(j)) yield
        if (i == 9 && j == 0) new CTextField()
        else if (i == 9) new CTextField(col.toString)
        else if (j == 0) new CTextField(row.toString)
        else {
          val color: BoardColor = if (i % 2 == j % 2) Brown.White else Brown.Black
          val pos = Sqr(col, row)
          new SquareButton(color, pos, chessBoard(pos))
        }
    }
    contents foreach (comp => listenTo(comp))
  }

  def unselect(square: Sqr): Unit =
    getSquareOnCoordinate(square) match {
      case Some(s) => s.unselect()
      case _ =>
    }

  private def getSquareOnCoordinate(square: Sqr): Option[SquareButton] =
    if (board.squares.isValid(square)) {
      val row = 9 - square.row
      val col = square.column + 1
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

  def displayCheck(on: Sqr): Unit =
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

  override def load(filePath: String): Option[FileOperationError] =
    if (filePath.nonEmpty) {
      lastSavePath = filePath
      super.load(filePath)
    } else Some(FileNotFoundError(filePath))

  def save(): Unit = super.save("")

  def saveAs(): Unit = {
    val fileChooser = new SaveFileChooser()
    val result = fileChooser.show(window)
    result match {
      case f: FileChooser.Result.Value if f == FileChooser.Result.Approve =>
        super.save(fileChooser.filePath)
      case _ =>
    }
  }
}
