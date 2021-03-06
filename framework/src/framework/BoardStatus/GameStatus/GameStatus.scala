package framework.BoardStatus.GameStatus

import framework.BoardStatus.GameResult.GameResult
import framework.FileOperationError.GameStatusLoadingError
import framework._

/**
  * The status of a [[framework.ChessBoard]]
 *
  * @version alpha 0.3
  * @author Felix Lehner
  */
sealed trait GameStatus {
  def isActive: Boolean
}

object GameStatus {
  def apply(state: String): Either[GameStatusLoadingError, GameStatus] = {
    import framework.BoardStatus.GameResult.GameResult.contains

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
      val result = Sqr(sqr)
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
case object StandardReq extends Active {
  override def toString: String = "Waiting"
}

/** Waiting for a promotion */
case class PromoReq(on: Sqr) extends Active

/** Waiting for an answer to a draw offer. */
case object DrawAcceptanceReq extends Active

/** Waiting for an answer to a takeback request. */
case object TakebackAcceptanceReq extends Active
