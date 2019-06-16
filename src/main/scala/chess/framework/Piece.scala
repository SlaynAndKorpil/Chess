package chess.framework

import scala.xml._

sealed trait Piece {
  val color: Color
  val moved: Boolean

  val identifier: Char

  val value: Int

  def toXml: Elem =
    <piece>
      <id>
        {identifier}
      </id>
      <color>
        {color}
      </color>
      <moved>
        {moved}
      </moved>
    </piece>

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  //TODO needs rework, should not depend on uniquity of the identifier
  def ===[T <: Piece] (other: T): Boolean =
    this.identifier == other.identifier && this.color == other.color

  override def toString: String = " " + identifier + (color match {
    case White => "w"
    case Black => "b"
    case _ => " "
  })
}

case object NoPiece extends Piece {
  /** @note only for internal use; simply returns this object */
  private[chess] def apply(c: Color, b: Boolean): NoPiece.type = this

  override def ===[T <: Piece](other: T): Boolean = other == NoPiece

  val color: Color = NoColor

  val identifier = ' '

  override val isEmpty: Boolean = true

  val moved: Boolean = false

  val value = 0
}

sealed abstract class AnyPiece(override val identifier: Char) extends Piece {
  override def isEmpty = false

  val color: AnyColor
}

case class Pawn(color: AnyColor, moved: Boolean = false) extends AnyPiece('P') {
  val value = 1
}

case class Bishop(color: AnyColor, moved: Boolean = false) extends AnyPiece('B') {
  val value = 3
}

case class Knight(color: AnyColor, moved: Boolean = false) extends AnyPiece('N') {
  val value = 3
}

case class Rook(color: AnyColor, moved: Boolean = false) extends AnyPiece('R') {
  val value = 5
}

case class Queen(color: AnyColor, moved: Boolean = false) extends AnyPiece('Q') {
  val value = 9
}

case class King(color: AnyColor, moved: Boolean = false) extends AnyPiece('K') {
  //evaluation according to Dr. Emanual Lasker
  val value: Int = 4
}


object Piece {
  def apply(id: Char, col: Color, moved: Boolean): Piece = col match {
    case any: AnyColor => id match {
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
