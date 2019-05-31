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

  private[framework] var board: ChessBoard

  def receiveInput (input: Input[_]): Unit = {
    val res = board.receive(input)
    res match {
      case Some(data) =>
        board = data._1
        data._2()
      case None =>
    }
  }
}
