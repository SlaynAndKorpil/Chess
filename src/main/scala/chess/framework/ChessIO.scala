package chess.framework

/**
  * @since alpha 0.1
  * @author Felix Lehner
  */
trait ChessIO {
  def showDrawOffer(): Unit

  def removeDrawOffer(): Unit

  def showPromotion(): Unit

  def removePromotion(): Unit

  def showTakeback(): Unit

  def removeTakeback(): Unit

  def showEnded(result: GameStatus.GameResult)
}
