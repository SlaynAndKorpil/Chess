package chess.framework

import chess.framework.IOEvents._
import chess.framework.Input._

/**
  * An interface to an [[chess.framework.ChessBoard]] for any I/O implementation.
  * This interface contains some convenience methods for easier interaction with
  * the board as well as methods that are used by the board to cause side-effects.
  *
  * Example implementation:
  * {{{
  *   class MyChessBoard extends ChessIO {
  *     override def update(): Unit = println(board.toString)
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
  * @see [[chess.framework.JavaInterfacing.JChessIO the java version]]
  * @since alpha 0.1
  * @author Felix Lehner
  */
trait ChessIO {
  /**
    * An implicit self-reference used by all constructors and
    * generators of [[chess.framework.ChessBoard]] to ease their use.
    *
    * @note this should not be overridden
    */
  protected implicit val io: this.type = this

  /**
    * The board that is used as an internal representation of the data structure.
    * This variable is mutable because [[chess.framework.ChessBoard]] is not although
    * mutability is needed for a program that can react to input.
    *
    * @see [[chess.framework.ChessBoard#classicalBoard]]
    */
  protected var board: ChessBoard

  /**
    * All available reactions to input.
    * @note You should not override this. Instead use the [[chess.framework.IOEvents.BoardReactions#+=]]
    *       or the [[chess.framework.IOEvents.BoardReactions#++=]] methods to add reactions.
    * @see [[chess.framework.IOEvents events]]
    * @usecase Used by the `receiveInput` method to find reactions to any event occurring.
    */
  protected val chessReactions: BoardReactions = new BoardReactions()

  /**
    * This method should update the output (e.g a GUI) and reload the data
    * from the [[chess.framework.ChessIO#board board]] into the data structure you are using.
    *
    * @note This should always clear any visual indication of a check as there is no event for this
    *       because every king checked won't be checked after the next move (i.e. no legal move
    *       of a checked player will ever result in being checked again.
    *
    * @usecase this method gets called by the `receiveInput` method whenever the board gets updated.
    */
  protected def update(): Unit

  /**
    * This redirects any input to the board and unpacks and processes the output.
    *
    * @usecase This method is mostly for convenience so that implementations of
    *          this trait do not have to process the result themselves.
    * @param input any form of [[chess.framework.Input]]
    */
  protected def receiveInput(input: Input[_]): Unit = {
    val res = board.receive(input)
    res match {
      case Some(data) =>
        board = data.board
        update()
        data.events foreach (event => chessReactions(event))
      case None =>
    }
  }
}
