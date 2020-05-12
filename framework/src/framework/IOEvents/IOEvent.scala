package framework.IOEvents

import framework.BoardStatus.GameResult.GameResult
import framework.Sqr

/**
  * An event given by [[framework.ChessBoard]] as reaction to specific inputs.
 *
  * @version alpha 0.2
  * @author Felix Lehner
  */
sealed trait IOEvent

case object ShowDrawOffer extends IOEvent

case object RemoveDrawOffer extends IOEvent

final case class ShowPromotion(on: Sqr) extends IOEvent

case object RemovePromotion extends IOEvent

case object ShowTakeback extends IOEvent

case object RemoveTakeback extends IOEvent

final case class ShowEnded(result: GameResult) extends IOEvent

final case class ShowCheck(on: Sqr) extends IOEvent

/** Placeholder event. */
case object NoEvent extends IOEvent
