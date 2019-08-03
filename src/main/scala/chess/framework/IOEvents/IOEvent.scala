package chess.framework.IOEvents

import chess.framework.BoardStatus.GameResult.GameResult
import chess.framework.Square

/**
  * An event given by [[chess.framework.ChessBoard]] as reaction to specific inputs.
  * @version alpha 0.2
  * @author Felix Lehner
  */
sealed trait IOEvent

case object ShowDrawOffer extends IOEvent

case object RemoveDrawOffer extends IOEvent

final case class ShowPromotion(on: Square) extends IOEvent

case object RemovePromotion extends IOEvent

case object ShowTakeback extends IOEvent

case object RemoveTakeback extends IOEvent

final case class ShowEnded(result: GameResult) extends IOEvent

final case class ShowCheck(on: Square) extends IOEvent

/** Placeholder event. */
case object NoEvent extends IOEvent
