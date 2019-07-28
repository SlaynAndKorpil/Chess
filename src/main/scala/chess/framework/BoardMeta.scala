package chess.framework

import chess.framework.BoardStatus.GameResult.Win
import chess.framework.BoardStatus.GameStatus.{Ended, GameStatus, PromoReq, StandardReq}
import chess.framework.BoardStatus.ResultReason.Resignation
import chess.framework.IOEvents._

import scala.language.postfixOps

/**
  * Meta information and methods that are needed for a game.
  * @usecase this gets mixed-in in [[chess.framework.ChessBoard ChessBoard]]
  * @author Felix Lehner
  * @version alpha 0.1
  */
trait BoardMeta {
  self: ChessBoard =>

  /** A list of all played moves. */
  val history: List[MoveData]

  /** A list of all former positions */
  val positions: Positions

  /** The status the game is currently in */
  val gameStatus: GameStatus

  /**
    * Takes the last move back.
    * If any of the players was checked, also a [[chess.framework.IOEvents.ShowCheck ShowCheck]] event is triggered.
    */
  private[framework] def takeback: Option[Output] =
    if (positions.length >= 1) {
      val resBoard = clone(
        squares = positions.head.pos,
        positions = positions --,
        history = history.tail,
        gameStatus = StandardReq,
        turn = turn.opposite)
      val takebackCheckEvents = resBoard.doOnCheck(pos => ShowCheck(pos), NoEvent)
      Output(
        resBoard,
        Array(RemoveTakeback) ++ takebackCheckEvents
      ) asSome
    }
    else None


  /**
    * Promotes a pawn to a given piece.
    *
    * @param piece the piece's apply method
    * @return an updated [[chess.framework.ChessBoard ChessBoard]] of [[scala.None]] when the piece type is incorrect
    */
  private[framework] def promote(piece: (AnyColor, Boolean) => AnyPiece): Option[Output] = {
    val promoColor = turn.opposite
    val promoPiece = piece match {
      case Queen =>
        Queen(promoColor)
      case Bishop =>
        Bishop(promoColor)
      case Knight =>
        Knight(promoColor)
      case Rook =>
        Rook(promoColor)
      case _ => NoPiece
    }

    if (promoPiece != NoPiece) {
      gameStatus match {
        case PromoReq(sqr: Square) =>
          val result = updated(sqr, promoPiece).clone(gameStatus = StandardReq)
          val promoCheckEvents: IndexedSeq[IOEvent] = result.doOnCheck[Array[IOEvent]](pos => Array(ShowCheck(pos)), Array()).flatten
          Output(result, RemovePromotion +: promoCheckEvents) asSome
        case _ => None
      }
    }
    else None
  }

  /**
    * Resigns the game, i.e. grants the win to the opposite color.
    */
  private[framework] def resign: Ended =
    Ended(Win(turn.opposite)(Resignation))
}
