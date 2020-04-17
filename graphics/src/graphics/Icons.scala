package graphics

import graphics.{assetsDir, piecesIconDir}
import java.awt.image.BufferedImage

import framework.{AnyPiece, Bishop, Black, King, Knight, Pawn, Queen, Rook, White}
import javax.imageio.ImageIO

/**
  * Loads and contains icons for all chess pieces.
  */
//TODO exception-handling!!!
object Icons {
  val dirs: scala.xml.Elem = xml.XML.load(getClass.getResourceAsStream(piecesIconDir))

  /**
    * Splits an image horizontally into 6 equally sized parts.
    * These should show the 6 different chess pieces in this order: King, Queen, Bishop, Knight, Rook, Pawn
    * @see [[blackImages]]; [[whiteImages]]
    * @return a sequence of these 6 images as [[java.awt.image.BufferedImage]]s
    */
  private def splitImg (image: BufferedImage): Seq[BufferedImage] =
    for (i <- 0 until 6) yield image.getSubimage(i*image.getWidth/6, 0, image.getWidth/6, image.getHeight)

  // TODO unnecessary mem alloc, sequences should be deleted after loading => put in method scope
  private val blackImages: Seq[BufferedImage] = splitImg(ImageIO.read(getClass.getResourceAsStream(assetsDir + (dirs \ "BlackPieces" \@ "dir"))))
  private val whiteImages: Seq[BufferedImage] = splitImg(ImageIO.read(getClass.getResourceAsStream(assetsDir + (dirs \ "WhitePieces" \@ "dir"))))

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
    * Assigns for every piece the correct [[java.awt.image.BufferedImage]]
    */
  def icon (p: AnyPiece): BufferedImage = p match {
    case Pawn(Black, _) => PawnBlack
    case Pawn(White, _) => PawnWhite
    case Bishop(Black, _) => BishopBlack
    case Bishop(White, _) => BishopWhite
    case Knight(Black, _) => KnightBlack
    case Knight(White, _) => KnightWhite
    case Rook(Black, _) => RookBlack
    case Rook(White, _) => RookWhite
    case Queen(Black, _) => QueenBlack
    case Queen(White, _) => QueenWhite
    case King(Black, _) => KingBlack
    case King(White, _) => KingWhite
  }
}
