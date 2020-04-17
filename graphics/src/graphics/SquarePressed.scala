package graphics

import scala.swing.event.ActionEvent

/**
  * An event-sth. for [[SquareButton]]s
  *
  * @param source the clicked square
  */
case class SquarePressed(override val source: SquareButton) extends ActionEvent(source)
