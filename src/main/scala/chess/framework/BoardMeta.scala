package chess.framework

import chess.framework.GameStatus.GameStatus

trait BoardMeta {
  self: ChessBoard =>

  val history: List[MoveData]

  val positions: Positions

  val gameStatus: GameStatus
}
