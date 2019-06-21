package chess.graphics

import java.awt.image.BufferedImage

import chess.framework.{AnyPiece, NoPiece, Piece}

import scala.swing._
import scala.swing.event._

/**
  * An abstract definition of a button that publishes a special event
  * when clicked and contains a piece that gets displayed when painted.
  *
  * @author Felix Lehner
  * @version alpha 0.1
  */
trait PieceButton extends AbstractButton {
  val eventType: this.type => ActionEvent
  var piece: Piece

  background = color

  /** The color used for the background */
  var color: BoardColors.BoardColor

  /**
    * Publishes every event it receives after replacing every [[scala.swing.event.ButtonClicked]] event
    * with an event of the [[chess.graphics.PieceButton#eventType eventType]] containing the same square.
    *
    * @param e any event
    */
  override def publish(e: Event): Unit =
    super.publish(e match {
      case ButtonClicked(sq: this.type) =>
        eventType(sq)
      case _ => e
    })

  /**
    * Paints this component like any [[scala.swing.AbstractButton]]
    * and then paints an image representing the piece type on top of it.
    */
  override def paintComponent(g: Graphics2D): Unit = {
    super.paintComponent(g)
    val image: Image = piece match {
      case somePiece: AnyPiece =>
        val img = Icons.icon(somePiece)
        scaledImgToButtonSize(img)
      case NoPiece => null
    }
    g.drawImage(image, 0, 0, peer)
  }

  /**
    * Resizes an image to the current size of this button
    * by scaling it relatively.
    *
    * @param image the image to resize
    * @return an image of the size of this button
    */
  private def scaledImgToButtonSize(image: BufferedImage): Image = {
    val result: BufferedImage = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)

    val xScale: Float = (image.getWidth.toDouble / result.getWidth.toDouble).toFloat
    val yScale: Float = (image.getHeight.toDouble / result.getHeight.toDouble).toFloat

    for (x <- 0 until result.getWidth; y <- 0 until result.getHeight)
      result.setRGB(x, y, image.getRGB(math.round(x * xScale), math.round(y * yScale)))

    result
  }

  /**
    * The current/runtime width of this button.
    */
  def width: Int = peer.getWidth

  /**
    * The current/runtime height of this button.
    */
  def height: Int = peer.getHeight
}
