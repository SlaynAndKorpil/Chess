package chess.framework.BoardStatus.GameStatus

import chess.framework.BoardStatus.GameResult.GameResult
import chess.framework._

sealed trait GameStatus {
  def isActive: Boolean
}

object GameStatus {
  def apply(state: String): Either[GameStatus, String] = {
    import chess.framework.BoardStatus.GameResult.GameResult.contains

    val last = state.length - 1
    val failMessage = Right("Failed to load gameStatus.")

    if (contains(state, "Ended(", ")")) {
      val result = state.substring(6, last)
      val res: Option[GameResult] = GameResult(result)
      res match {
        case Some(x) => Left(Ended(x))
        case _ => failMessage
      }
    }
    else if (state == StandardReq.toString) Left(StandardReq)
    else if (contains(state, "PromoReq(", ")")) {
      val sqr = state.substring(9, last)
      val result = Square(sqr)
      result match {
        case Some(square) => Left(PromoReq(square))
        case _ => failMessage
      }
    }
    else if (state == DrawAcceptanceReq.toString) Left(DrawAcceptanceReq)
    else if (state == TakebackAcceptanceReq.toString) Left(TakebackAcceptanceReq)
    else failMessage
  }
}

case class Ended(result: GameResult) extends GameStatus {
  def isActive: Boolean = false
}

sealed trait Active extends GameStatus {
  def isActive: Boolean = true
}

object StandardReq extends Active {
  override def toString: String = "Waiting"
}

case class PromoReq(square: Square) extends Active

object DrawAcceptanceReq extends Active {
  override def toString: String = "Draw reaction"
}

object TakebackAcceptanceReq extends Active {
  override def toString: String = "TakebackAcceptanceReq"
}
