package chess.framework

import chess.framework.BoardStatus.GameResult._
import chess.framework.Input._

/**
  * An interface to an [[chess.framework.ChessBoard]] for any I/O implementation.
  * This interface contains some convenience methods for easier interaction with
  * the board as well as methods that are used by the board to cause side-effects.
  *
  * @since alpha 0.1
  * @author Felix Lehner
  */
trait ChessIO {
  /**
    * An implicit self-reference used by all constructors and
    * generators of [[chess.framework.ChessBoard]] to ease their use.
    */
  implicit val io: this.type = this

  /**
    * The board that is used as an internal representation of the data structure.
    *
    * @note You can also rewrite the getter- and setter- method of this to redirect to something else.
    * @see [[chess.framework.ChessBoard#classicalBoard]]
    */
  private[framework] var board: ChessBoard

  /** This is called when the player should decide if he agrees to a draw. */
  def showDrawOffer(): Unit

  /** This is called when the draw offer should be closed. */
  def removeDrawOffer(): Unit

  /** This is called when the board expects a [[chess.framework.Input.Promotion]] as input. */
  def showPromotion(): Unit

  /** This is called when the promotion option should disappear. */
  def removePromotion(): Unit

  /** This is called when a takeback is proposed. */
  def showTakeback(): Unit

  /** This is called when the takeback option should disappear. */
  def removeTakeback(): Unit

  /**
    * This method gets called when the current game ended to display the result.
    *
    * @param result the result that can/ should be displayed to the user.
    */
  def showEnded(result: GameResult)

  def showCheck(on: SquareCoordinate): Unit

  /**
    * This method should update the output (e.g a GUI) and reload the data
    * from the [[chess.framework.ChessIO#board]] into the data structure you are using.
    *
    * @usecase this method gets called by the `receiveInput` method whenever the board gets updated.
    */
  def update(): Unit

  /**
    * This redirects any input to the board and unpacks and processes the output.
    *
    * @usecase This method is mostly for convenience so that implementations of
    *          this trait do not have to process the result themselves.
    * @param input any form of [[Input]]
    */
  def receiveInput(input: Input[_]): Unit = {
    val res = board.receive(input)
    res match {
      case Some(data) =>
        board = data._1
        update()
        data._2()
      case None =>
    }
  }
}
