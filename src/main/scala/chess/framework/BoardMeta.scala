package chess.framework

import chess.framework.GameStatus._

trait BoardMeta {
  self: ChessBoard =>

  val history: List[MoveData]

  val positions: Positions

  val gameStatus: GameStatus

  private[framework] def takeback: Option[(ChessBoard, () => Unit)] =
    if (positions.length >= 1) {
      Some(
        clone(squares = positions.head.pos, positions = this.positions.--, gameStatus = StandardReq, turn = this.turn.opposite),
        () => io.removeTakeback()
      )
    }
    else None


  /**
    * Promotes a pawn to a given piece.
    *
    * @param piece the piece's apply method
    * @return an updated [[ChessBoard]] of [[None]] when the piece type is incorrect
    */
  private[framework] def promote(piece: (AnyColor, Boolean) => AnyPiece): Option[(ChessBoard, () => Unit)] = {
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
        case PromoReq(sqr: SquareCoordinate) =>
          val result = updated(sqr, promoPiece).clone(gameStatus = StandardReq)
          Some(result, () => io.removePromotion())
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
