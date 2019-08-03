package chess.framework.BoardStatus.GameStatus

import chess.framework.BoardStatus.GameResult.GameResult
import chess.framework.LoadingError.GameStatusLoadingError
import chess.framework._

/**
  * The status of a [[chess.framework.ChessBoard]]
  * @version alpha 0.2
  * @author Felix Lehner
  */
sealed trait GameStatus {
  def isActive: Boolean
}

object GameStatus {
  def apply(state: String): Either[GameStatusLoadingError, GameStatus] = {
    import chess.framework.BoardStatus.GameResult.GameResult.contains

    val last = state.length - 1
    val failMessage = Left(GameStatusLoadingError(state))

    if (contains(state, "Ended(", ")")) {
      val result = state.substring(6, last)
      val res: Option[GameResult] = GameResult(result)
      res match {
        case Some(x) => Right(Ended(x))
        case _ => failMessage
      }
    }
    else if (state == StandardReq.toString) Right(StandardReq)
    else if (contains(state, "PromoReq(", ")")) {
      val sqr = state.substring(9, last)
      val result = Square(sqr)
      result match {
        case Some(square) => Right(PromoReq(square))
        case _ => failMessage
      }
    }
    else if (state == DrawAcceptanceReq.toString) Right(DrawAcceptanceReq)
    else if (state == TakebackAcceptanceReq.toString) Right(TakebackAcceptanceReq)
    else failMessage
  }
}

/** An ended game */
final case class Ended(result: GameResult) extends GameStatus {
  def isActive: Boolean = false
}

/** The game is still active*/
sealed trait Active extends GameStatus {
  def isActive: Boolean = true
}

/** Waiting for standard input(move, draw offer, ...). */
object StandardReq extends Active {
  override def toString: String = "Waiting"
}

/** Waiting for a promotion */
case class PromoReq(on: Square) extends Active

/** Waiting for an answer to a draw offer. */
object DrawAcceptanceReq extends Active {
  override def toString: String = "Draw reaction"
}

/** Waiting for an answer to a takeback request. */
object TakebackAcceptanceReq extends Active {
  override def toString: String = "TakebackAcceptanceReq"
}
