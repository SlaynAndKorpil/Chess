package framework

/**
  * A chess piece.
  *
  * @version alpha 0.2
  * @author Felix Lehner
  */
sealed trait Piece {
  /** The color. */
  val color: Color

  /** Indicates weather this piece was moved. */
  val moved: Boolean

  val identifier: Char

  val value: Int

  /** Saves this piece as XML. */
  def toXml: scala.xml.Elem = <piece id={toString} moved={moved.toString}/>

  /** Indicates if this is an empty square. */
  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  /** Compares this with another piece only under consideration of the piece type (identifier and color). */
  def ===[T <: Piece](other: T): Boolean =
    this.identifier == other.identifier && this.color == other.color

  /** Negation of [[framework.Piece#===]] */
  def !==[T <: Piece](other: T): Boolean =
    this.identifier != other.identifier || this.color != other.color

  override def toString: String = color match {
    case White => identifier.toLower.toString
    case Black => identifier.toUpper.toString
    case _ => " "
  }
}

/**
  * Represents an empty square.
  */
case object NoPiece extends Piece {
  override val isEmpty: Boolean = true
  val color: Color = NoColor
  val identifier = ' '
  val moved: Boolean = false
  val value = 0

  override def ===[T <: Piece](other: T): Boolean = other == NoPiece

  /** @note only for internal use; simply returns this object */
  private[framework] def apply(c: Color, b: Boolean): NoPiece.type = this
}

/**
  * A non-empty square.
  */
sealed abstract class AnyPiece(override val identifier: Char, override val value: Int) extends Piece {
  val color: AnyColor

  override def isEmpty = false
}

case class Pawn(color: AnyColor, moved: Boolean = false) extends AnyPiece('P', 1)

case class Bishop(color: AnyColor, moved: Boolean = false) extends AnyPiece('B', 3)

case class Knight(color: AnyColor, moved: Boolean = false) extends AnyPiece('N', 3)

case class Rook(color: AnyColor, moved: Boolean = false) extends AnyPiece('R', 5)

case class Queen(color: AnyColor, moved: Boolean = false) extends AnyPiece('Q', 9)

case class King(color: AnyColor, moved: Boolean = false) extends AnyPiece('K', 4 /*evaluation according to Dr. Emanual Lasker*/)

object Piece {
  def apply(id: Char, col: Color, moved: Boolean): Piece = col match {
    case any: AnyColor => id.toUpper match {
      case 'P' => Pawn(any, moved)
      case 'B' => Bishop(any, moved)
      case 'N' => Knight(any, moved)
      case 'R' => Rook(any, moved)
      case 'Q' => Queen(any, moved)
      case 'K' => King(any, moved)
      case _ => NoPiece
    }
    case NoColor => NoPiece
  }

  def apply(id: Char): Option[(AnyColor, Boolean) => AnyPiece] = id.toUpper match {
    case 'P' => Some(Pawn.apply)
    case 'B' => Some(Bishop.apply)
    case 'N' => Some(Knight.apply)
    case 'R' => Some(Rook.apply)
    case 'Q' => Some(Queen.apply)
    case 'K' => Some(King.apply)
    case _ => None
  }
}
