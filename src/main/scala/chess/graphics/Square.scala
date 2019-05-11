package chess.graphics

import java.awt.image.{BufferedImage, ImageObserver}
import chess.framework.SquareCoordinate
import chess.graphics.BoardColors.BoardColor
import scala.swing.{Graphics2D, Insets, ToggleButton, Image}
import scala.swing.event.{ButtonClicked, Event}

class Square (color: BoardColor, val pos: SquareCoordinate, var piece: chess.framework.Piece) extends ToggleButton {
//a small hack that changes every ButtonClicked in a SquarePressed event before publishing
  override def publish(e: Event): Unit = super.publish(e match {
    case ButtonClicked(sq: Square) =>
      SquarePressed(sq)
    case _ => e
  })

//  color for text
  foreground = BoardColors.LightGray.Black

//  color of background
  background = color

//  space between the borders and the content/text
  margin = new Insets(100, 100, 100, 100)

//  contentAreaFilled = true; rendering the background(images, color)

//  selection/ being clicked; doClick() takes 60 ms to perform
//  selectedIcon = icon
  def isSelected: Boolean = peer.isSelected
  def unselect(): Unit = if (isSelected) setUnselected()
  def setUnselected(): Unit = {
    val model = peer.getModel
    model.setSelected(false)
    peer.setModel(model)
  }

//  width and height of ?
  def width: Int = peer.getWidth
  def height: Int = peer.getHeight

//  ?
  requestFocusInWindow()

  private def resizeImage (image: BufferedImage, observer: ImageObserver): Image = {
    val img: BufferedImage = image
    val result: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val xScale: Double = img.getWidth.toDouble / result.getWidth.toDouble
    val yScale: Double = img.getHeight.toDouble / result.getHeight.toDouble

    for (x <- 0 until result.getWidth; y <- 0 until result.getHeight)
      result.setRGB(x, y, img.getRGB((x * xScale).toInt, (y * yScale).toInt))

    result
  }

  override def paintComponent (g: Graphics2D): Unit = {
    super.paintComponent(g)
    val image: Image = Icons.icon(piece) match {
      case Some(img) => resizeImage(img, peer)
      case None => null
    }
    g.drawImage(image, 0, 0, peer)
  }
}
