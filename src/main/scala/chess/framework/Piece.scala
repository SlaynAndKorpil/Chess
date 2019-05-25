package chess.framework

import scala.xml._

sealed trait Piece {
  val color: Color
  val moved: Boolean

  val identifier: Char

  def xml: Elem =
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

  override def toString: String = " " + identifier + (color match {
    case White => "w"
    case Black => "b"
    case _ => " "
  })
}

object NoPiece extends Piece {
  /** @note only for internal use; simply returns this object */
  private[chess] def apply(c: Color, b: Boolean): NoPiece.type = this

  val color: Color = NoColor

  val identifier = ' '

  override val isEmpty: Boolean = true

  val moved: Boolean = false
}

sealed abstract class AnyPiece(override val identifier: Char) extends Piece {
  override def isEmpty = false

  val color: AnyColor
}

final case class Pawn(color: AnyColor, moved: Boolean = false) extends AnyPiece('P')

final case class Bishop(color: AnyColor, moved: Boolean = false) extends AnyPiece('B')

final case class Knight(color: AnyColor, moved: Boolean = false) extends AnyPiece('N')

final case class Rook(color: AnyColor, moved: Boolean = false) extends AnyPiece('R')

final case class Queen(color: AnyColor, moved: Boolean = false) extends AnyPiece('Q')

final case class King(color: AnyColor, moved: Boolean = false) extends AnyPiece('K')


object Piece {
  def fromXML(xml: NodeSeq): Piece = {
    import SaveLoader.extractWithFilter
    if (xml.isEmpty || xml.head.isEmpty) NoPiece
    else {
      val data = xml.head
      apply(extractWithFilter(data, "id").head, Color(extractWithFilter(data, "color")), extractWithFilter(data, "moved").toBoolean)
    }
  }

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
}
