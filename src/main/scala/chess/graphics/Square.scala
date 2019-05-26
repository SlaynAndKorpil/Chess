package chess.graphics

import chess.framework.SquareCoordinate
import chess.graphics.BoardColors.BoardColor

import scala.swing.event.ActionEvent
import scala.swing.{Graphics2D, Image, Insets, ToggleButton}

class Square(
              override var color: BoardColor,
              val pos: SquareCoordinate,
              override var piece: chess.framework.Piece)
  extends ToggleButton with PieceButton {

  //space between the borders and the content/text
  margin = new Insets(100, 100, 100, 100)

  //contentAreaFilled = true; rendering the background(images, color)

  override val eventType: Square.this.type => ActionEvent = SquarePressed

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
