package chess.framework

import scala.xml.{Elem, NodeSeq}

/**
  * Information about the position the game started with.
  *
  * @author Felix Lehner
  * @version
  */
trait StartPosition {
  def xml: NodeSeq

  def squares: BoardMap
}

/**
  * Represents the classic chess start position.
  */
object ClassicPosition extends StartPosition {
  val xml: Elem = <ClassicPosition/>

  val squares: BoardMap = ChessBoard.classicalPosition
}

/**
  * Some arbitrary start position.
  *
  * @param squares the pieces
  */
case class ArbitraryPosition(squares: BoardMap) extends StartPosition {
  def xml: NodeSeq = ChessBoard.saveSquares(squares)
}
