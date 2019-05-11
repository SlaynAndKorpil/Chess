package chess.framework

trait ChessIO {
  def showDrawOffer(): Unit

  def removeDrawOffer(): Unit

  def showPromotion(): Unit

  def removePromotion(): Unit

  def showResign(): Unit
}
