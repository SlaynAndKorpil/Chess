package chess.framework.GameStatus

import chess.framework._

sealed trait GameStatus {
  def isActive: Boolean
}

case class Ended(result: GameResult) extends GameStatus {
  def isActive: Boolean = false
}

trait Active extends GameStatus {
  def isActive: Boolean = true
}

object StandardReq extends Active

case class PromoReq(square: SquareCoordinate) extends Active

object DrawAcceptanceReq extends Active
