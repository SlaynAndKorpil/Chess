package chess.framework

import scala.xml.Elem

sealed trait Piece {
  val color: Color
  var moved: Boolean

  val identifier: Char

  def xml: Elem =
    <piece>
      <id>{identifier}</id>
      <color>{color}</color>
      <moved>{moved}</moved>
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
  /**@note only for internal use; simply returns this object*/
  private[chess] def apply(c: Color, b: Boolean): NoPiece.type = this

  val color: Color = NoColor

  val identifier = ' '

  override val isEmpty: Boolean = true

  val moved: Boolean = false

  def moved_= (b: Boolean): Unit = ()
}

sealed abstract class AnyPiece (override val identifier: Char) extends Piece {
  override def isEmpty = false
  val color: Color
}

final case class Pawn (color: Color, var moved: Boolean = false) extends AnyPiece('P')

final case class Bishop (color: Color, var moved: Boolean = false) extends AnyPiece('B')

final case class Knight (color: Color, var moved: Boolean = false) extends AnyPiece('N')

final case class Rook (color: Color, var moved: Boolean = false) extends AnyPiece('R')

final case class Queen (color: Color, var moved: Boolean = false) extends AnyPiece('Q')

final case class King (color: Color, var moved: Boolean = false) extends AnyPiece('K')



object Piece {
  def apply (id: Char): (Color, Boolean) => Piece = id match {
    case 'P' => Pawn
    case 'B' => Bishop
    case 'N' => Knight
    case 'R' => Rook
    case 'Q' => Queen
    case 'K' => King
    case _ => NoPiece.apply
  }
}
