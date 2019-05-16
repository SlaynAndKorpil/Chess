package chess.graphics

import java.awt.image.{BufferedImage, ImageObserver}

import chess.framework.Piece

import scala.swing._
import scala.swing.event._

trait PieceButton extends AbstractButton {
  var piece: Piece
  var color: BoardColors.BoardColor

  background = color
  foreground = color

  val eventType: this.type => ActionEvent

  override def publish(e: Event): Unit =
    super.publish(e match {
      case ButtonClicked(sq: this.type) =>
        eventType(sq)
      case _ => e
    })


  //width and height of actual square
  def width: Int = peer.getWidth

  def height: Int = peer.getHeight

  private def resizeImage(image: BufferedImage, observer: ImageObserver): Image = {
    val img: BufferedImage = image
    val result: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val xScale: Double = img.getWidth.toDouble / result.getWidth.toDouble
    val yScale: Double = img.getHeight.toDouble / result.getHeight.toDouble

    for (x <- 0 until result.getWidth; y <- 0 until result.getHeight)
      result.setRGB(x, y, img.getRGB((x * xScale).toInt, (y * yScale).toInt))

    result
  }

  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val image: Image = Icons.icon(piece) match {
      case Some(img) => resizeImage(img, peer)
      case None => null
    }
    g.drawImage(image, 0, 0, peer)
  }
}
