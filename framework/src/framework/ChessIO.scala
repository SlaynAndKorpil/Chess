package framework

import IOEvents.BoardReactions
import framework.Input._

/**
  * An interface to an [[framework.ChessBoard]] for any I/O implementation.
  * This interface contains some convenience methods for easier interaction with
  * the board as well as methods that are used by the board to cause side-effects.
  *
  * Example implementation:
  * {{{
  *   class MyChessBoard extends ChessIO {
  *     override def update(): Unit = println(board.toString)
  *     override protected var board: ChessBoard = ChessBoard.classicalBoard
  *
  *     chessReactions += {
  *       case ShowDrawOffer =>
  *         println("Do you want a draw?")
  *       case ShowEnded(result) =>
  *         println(s"The game ended with a $result")
  *     }
  *   }
  *
  *   val test = new MyBoard()
  *
  *   test receiveInput DrawOffer
  *   test receiveInput DrawReject
  *   test receiveInput MoveParams(from = Square('d', 2), to = Square('d', 4))
  * }}}
  *
  * @see [[framework.javaInterfacing.JChessIO the java variant]]
  * @version alpha 0.3
  * @author Felix Lehner
  */
trait ChessIO {
  /**
    * An implicit self-reference used by all constructors and
    * generators of [[framework.ChessBoard]] to ease their use.
    *
    * @note this should not be overridden
    */
  protected implicit val io: this.type = this

  /**
    * All available reactions to input.
    *
    * @note You should not override this. Instead use the [[framework.IOEvents.BoardReactions#+=]]
    *       or the [[framework.IOEvents.BoardReactions#++=]] methods to add reactions.
    * @see [[framework.IOEvents events]]
    * @usecase Used by the `receiveInput` method to find reactions to any event occurring.
    */
  protected val chessReactions: BoardReactions = new BoardReactions()

  /**
    * The path to the directory where this game was saved the last time.
    * Used to override the last save without searching for the file.
    */
  protected var lastSavePath: String = "save"

  /**
    * The board that is used as an internal representation of the data structure.
    * This variable is mutable because [[framework.ChessBoard]] is not although
    * mutability is needed for a program that can react to input.
    *
    * @see [[framework.ChessBoard#classicalBoard]] for initialization
    */
  protected var board: ChessBoard

  /**
    * This method should update the output (e.g a GUI) and reload the data
    * from the [[framework.ChessIO#board board]] into the data structure you are using.
    *
    * @note This should always clear any visual indication of a check as there is no event for this
    *       because every king checked won't be checked after the next move (i.e. no legal move
    *       of a checked player will ever result in being checked again.)
    * @usecase this method is called whenever the chessboard gets updated.
    */
  protected def update(): Unit

  /**
    * This redirects any input to the board and unpacks and processes the output.
    *
    * @usecase This method is mostly for convenience so that implementations of
    *          this trait do not have to process the result themselves.
    * @param input any form of [[framework.Input]]
    */
  protected def receiveInput(input: Input[_]): Unit = {
    val res = board.receive(input)
    res match {
      case Some(data) =>
        input match {
          case DrawReject | DrawOffer | DrawAcceptance | TakebackReject | TakebackProposal | Resign =>
            board = data.board
          case _ =>
            chessBoard = data.board
        }
        data.events foreach (event => chessReactions(event))
      case None =>
    }
  }

  /**
    * Loads a saved game from a file.
    *
    * @param filePath The path to the file. An `.save` extension is added when there is none.
    * @return a [[framework.FileOperationError.FileOperationError FileOperationError]] if an error occurs in the parsing process
    */
  protected def load(filePath: String): Option[framework.FileOperationError.FileOperationError] =
    ChessBoard.load(filePath) match {
      case Right(loadedBoard) =>
        chessBoard = loadedBoard
        loadedBoard.doOnCheck(pos => chessReactions(IOEvents.ShowCheck(pos)), Unit)
        None
      case Left(error) => Some(error)
    }

  /**
    * The board with all pieces and meta data of the current game.
    *
    * @see [[framework.ChessBoard#classicalBoard]] for initialization
    */
  def chessBoard: ChessBoard = board

  /**
    * The board that is used as an internal representation of the data structure.
    * When changing it with this method the [[framework.ChessIO#update update]] method is called.
    *
    * @see [[framework.ChessBoard#classicalBoard]] for initialization
    */
  def chessBoard_=(board: ChessBoard): Unit = {
    this.board = board
    update()
  }

  /**
    * Saves the current game to a file.
    * Also overrides the [[framework.ChessIO#lastSavePath lastSavePath]] variable with the new path.
    *
    * @param filePath The file used to store the data.
    *                 `.save` is added when there is no file extension yet.
    *                 When empty, the last save path is used.
    */
  protected def save(filePath: String): Option[framework.FileOperationError.FileNotFoundError] = {
    if (filePath.nonEmpty && lastSavePath != filePath) lastSavePath = filePath
    ChessBoard.save(chessBoard, lastSavePath)
  }
}
