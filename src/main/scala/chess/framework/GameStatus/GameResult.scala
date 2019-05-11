package chess.framework.GameStatus

import chess.framework._

sealed trait GameResult {
  val reason: ResultReason
}

sealed trait Win extends GameResult {
  val reason: WinResultReason
}

object Win {
  def apply(winner: Color): WinResultReason => Win = winner match {
    case White => WhiteWins
    case Black => BlackWins
    case NoColor => WhiteWins//TODO not very fancy
  }
}

case class BlackWins(reason: WinResultReason) extends Win

case class WhiteWins(reason: WinResultReason) extends Win


case class Draw(reason: DrawResultReason) extends GameResult
