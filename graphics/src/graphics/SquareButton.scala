package graphics

import BoardColors._
import framework.Sqr

import scala.swing.event.ActionEvent
import scala.swing.{Graphics2D, Image, Insets, ToggleButton}

class SquareButton(
                    override var color: BoardColor,
                    val pos: Sqr,
                    override var piece: framework.Piece)
  extends ToggleButton with PieceButton {

  override val eventType: SquareButton.this.type => ActionEvent = SquarePressed
  var checked: Boolean = false

  //space between the borders and the content/text
  margin = new Insets(100, 100, 100, 100)

  override def paintComponent(g: Graphics2D): Unit = {
    background = if (checked) CheckedColor else color
    super.paintComponent(g)
  }

  def unselect(): Unit = if (isSelected) setUnselected()

  //selection/ being clicked; doClick() takes 60 ms to perform
  //selectedIcon = icon
  def isSelected: Boolean = peer.isSelected

  def setUnselected(): Unit = {
    val model = peer.getModel
    model setSelected false
    peer setModel model
  }
}
