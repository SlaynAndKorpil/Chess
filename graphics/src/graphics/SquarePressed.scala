package graphics

import SquareButton

/**
  * An event-sth. for [[SquareButton]]s
 *
  * @param source the clicked square
  */
case class SquarePressed(override val source: SquareButton) extends ActionEvent(source)
