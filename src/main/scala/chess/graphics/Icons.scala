package chess.graphics

import java.awt.image.BufferedImage
import java.io.File
import javax.swing.{Icon, ImageIcon}
import chess.framework._
import javax.imageio.ImageIO
import scala.xml.Elem

/**
  * Loads and contains icons for all chess pieces.
  */
//TODO exception-handling!!!
object Icons {
  val dirs: Elem = xml.XML.load(piecesIconDir)

  /**
    * Splits an image horizontally into 6 equally sized parts.
    * These should show the 6 different chess pieces in this order: King, Queen, Bishop, Knight, Rook, Pawn
    * @see [[blackImages]]; [[whiteImages]]
    * @return a sequence of these 6 images as [[ImageIcon]]s
    */
  private def splitImg (image: BufferedImage): Seq[BufferedImage] =
    for (i <- 0 until 6) yield image.getSubimage(i*image.getWidth/6, 0, image.getWidth/6, image.getHeight)

  private val blackImages: Seq[BufferedImage] = splitImg(ImageIO.read(new File(assetsDir + (dirs \ "BlackPieces" \@ "dir"))))
  private val whiteImages: Seq[BufferedImage] = splitImg(ImageIO.read(new File(assetsDir + (dirs \ "WhitePieces" \@ "dir"))))

  val PawnBlack: BufferedImage = blackImages(5)
  val PawnWhite: BufferedImage = whiteImages(5)
  val RookBlack: BufferedImage = blackImages(4)
  val RookWhite: BufferedImage = whiteImages(4)
  val KnightBlack: BufferedImage = blackImages(3)
  val KnightWhite: BufferedImage = whiteImages(3)
  val BishopBlack: BufferedImage = blackImages(2)
  val BishopWhite: BufferedImage = whiteImages(2)
  val QueenBlack: BufferedImage = blackImages(1)
  val QueenWhite: BufferedImage = whiteImages(1)
  val KingBlack: BufferedImage = blackImages.head
  val KingWhite: BufferedImage = whiteImages.head


  /**
    * Assigns for every piece the correct [[Icon]]
    */
  def icon (p: Piece): Option[BufferedImage] = p match {
    case Pawn(Black, _) => Some(PawnBlack)
    case Pawn(White, _) => Some(PawnWhite)
    case Bishop(Black, _) => Some(BishopBlack)
    case Bishop(White, _) => Some(BishopWhite)
    case Knight(Black, _) => Some(KnightBlack)
    case Knight(White, _) => Some(KnightWhite)
    case Rook(Black, _) => Some(RookBlack)
    case Rook(White, _) => Some(RookWhite)
    case Queen(Black, _) => Some(QueenBlack)
    case Queen(White, _) => Some(QueenWhite)
    case King(Black, _) => Some(KingBlack)
    case King(White, _) => Some(KingWhite)
    case _ => None
  }
}
