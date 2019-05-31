package chess.framework.GameStatus

import chess.framework._

sealed trait GameResult {
  val reason: ResultReason
}

object GameResult {
  def apply(reason: String): Option[GameResult] = {
    val last = reason.last
    if (contains(reason, "BlackWins(", ")"))
      createResult(reason.substring(10, last), BlackWins.apply)
    else if (contains(reason, "WhiteWins(", ")"))
      createResult(reason.substring(10, last), WhiteWins.apply)
    else if (contains(reason, "Draw(", ")"))
      createResult(reason.substring(5, last), Draw.apply)
    else None
  }

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
    case White => WhiteWins.apply
    case Black => BlackWins.apply
  }
}

case class BlackWins(reason: WinResultReason) extends Win

object BlackWins {
  def by(reason: WinResultReason): BlackWins = BlackWins(reason)
}

case class WhiteWins(reason: WinResultReason) extends Win

object WhiteWins {
  def by(reason: WinResultReason): WhiteWins = WhiteWins(reason)
}

case class Draw(reason: DrawResultReason) extends GameResult

object Draw {
  def by(reason: DrawResultReason): Draw = Draw(reason)
}
