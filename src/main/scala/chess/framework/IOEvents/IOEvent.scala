package chess.framework.IOEvents

import chess.framework.BoardStatus.GameResult.GameResult
import chess.framework.Square

sealed trait IOEvent

object ShowDrawOffer extends IOEvent

object RemoveDrawOffer extends IOEvent

case class ShowPromotion(on: Square) extends IOEvent

object RemovePromotion extends IOEvent

object ShowTakeback extends IOEvent

object RemoveTakeback extends IOEvent

case class ShowEnded(result: GameResult) extends IOEvent

case class ShowCheck(on: Square) extends IOEvent

object NoEvent extends IOEvent
