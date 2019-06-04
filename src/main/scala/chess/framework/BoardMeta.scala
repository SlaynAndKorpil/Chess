package chess.framework

import chess.framework.BoardStatus.GameResult.Win
import chess.framework.BoardStatus.GameStatus.{Ended, GameStatus, PromoReq, StandardReq}
import chess.framework.BoardStatus.ResultReason.Resignation
import chess.framework.IOEvents.{IOEvent, RemoveTakeback, RemovePromotion}

trait BoardMeta {
  self: ChessBoard =>

  val history: List[MoveData]

  val positions: Positions

  val gameStatus: GameStatus

  private[framework] def takeback: Option[(ChessBoard, IOEvent)] =
    if (positions.length >= 1) {
      Some(
        clone(
          squares = positions.head.pos,
          positions = positions.--,
          history = history.tail,
          gameStatus = StandardReq,
          turn = turn.opposite),
        RemoveTakeback
      )
    }
    else None


  /**
    * Promotes a pawn to a given piece.
    *
    * @param piece the piece's apply method
    * @return an updated [[ChessBoard]] of [[None]] when the piece type is incorrect
    */
  private[framework] def promote(piece: (AnyColor, Boolean) => AnyPiece): Option[(ChessBoard, IOEvent)] = {
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
          Some(result, RemovePromotion)
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
