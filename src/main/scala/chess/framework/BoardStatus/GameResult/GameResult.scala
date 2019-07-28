package chess.framework.BoardStatus.GameResult

import chess.framework.BoardStatus.ResultReason._
import chess.framework._

/** The result of an ended game. */
sealed trait GameResult {
  /** The reason of this result. */
  val reason: ResultReason
}

object GameResult {
  def apply(result: String): Option[GameResult] = {
    val last = result.length - 1
    if (contains(result, "BlackWins(", ")"))
      createResult(result.substring(10, last), BlackWins.apply)
    else if (contains(result, "WhiteWins(", ")"))
      createResult(result.substring(10, last), WhiteWins.apply)
    else if (contains(result, "Draw(", ")")) {
      createResult(result.substring(5, last), Draw.apply)
    }
    else None
  }

  private def createResult[R <: ResultReason](reason: String, func: R => GameResult): Option[GameResult] = {
    val param: Option[ResultReason] = ResultReason(reason)
    param match {
      case Some(x: R) => Some(func(x))
      case _ => None
    }
  }

  private[BoardStatus] def contains(in: String, start: String, end: String): Boolean =
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

final case class BlackWins(reason: WinResultReason) extends Win

object BlackWins {
  def by(reason: WinResultReason): BlackWins = BlackWins(reason)
}

final case class WhiteWins(reason: WinResultReason) extends Win

object WhiteWins {
  def by(reason: WinResultReason): WhiteWins = WhiteWins(reason)
}

final case class Draw(reason: DrawResultReason) extends GameResult

object Draw {
  def by(reason: DrawResultReason): Draw = Draw(reason)
}
