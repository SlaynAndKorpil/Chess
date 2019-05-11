package chess.graphics

import scala.swing.event._

/**
  * An event-sth. for [[Square]]s
  * @param source the clicked square
  */
case class SquarePressed(override val source: Square) extends ActionEvent(source)
