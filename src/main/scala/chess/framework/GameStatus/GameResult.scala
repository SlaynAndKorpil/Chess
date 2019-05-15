package chess.framework.GameStatus

import chess.framework._

sealed trait GameResult {
  val reason: ResultReason
}

object GameResult {
  def apply(reason: String): Option[GameResult] = {
    val last = reason.last
    if (contains(reason, "BlackWins(", ")"))
      createResult(reason.substring(10, last), BlackWins)
    else if (contains(reason, "WhiteWins(", ")"))
      createResult(reason.substring(10, last), WhiteWins)
    else if (contains(reason, "Draw(", ")"))
      createResult(reason.substring(5, last), Draw)
    else None
  }

  //TODO find name
  private def createResult[R <: ResultReason](reason: String, func: R => GameResult): Option[GameResult] = {
    val param: Option[ResultReason] = ResultReason(reason)
    param match {
      case Some(x: R) => Some(func(x))
      case _ => None
    }
  }

  def contains(in: String, start: String, end: String): Boolean =
    in.startsWith(start) && in.endsWith(end)
}

sealed trait Win extends GameResult {
  val reason: WinResultReason
}

object Win {
  def apply(winner: AnyColor): WinResultReason => Win = winner match {
    case White => WhiteWins
    case Black => BlackWins
  }
}

case class BlackWins(reason: WinResultReason) extends Win

case class WhiteWins(reason: WinResultReason) extends Win

case class Draw(reason: DrawResultReason) extends GameResult
