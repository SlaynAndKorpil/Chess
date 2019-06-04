package chess.graphics

import scala.swing.event._

/**
  * An event-sth. for [[SquareButton]]s
 *
  * @param source the clicked square
  */
case class SquarePressed(override val source: SquareButton) extends ActionEvent(source)
