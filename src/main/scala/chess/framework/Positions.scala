package chess.framework

import chess.framework.ChessBoard.columnLetter

import scala.language.postfixOps

/**
  * Saves all former positions of [[chess.framework.ChessBoard]]
  *
  * @param positions a list of known positions
  * @version alpha 0.2
  * @author Felix Lehner
  */
class Positions(val positions: IndexedSeq[Position], val maxRepetition: Int) {

  /** Adds a new position and updates the repetition counter. */
  def +(that: Position): Positions = {
    val maxRep: Int = {
      val len = positions count (that == _)
      if (len > maxRepetition) len
      else maxRepetition
    }
    Positions(that +: positions, maxRep)
  }

  /**
    * Deletes the last position.
    *
    * @usecase Useful for takebacks
    * @see [[chess.framework.ChessBoard]]
    */
  def -- : Positions = Positions(positions.tail)

  def apply(index: Int): Position = positions(index)

  /** @return the number of stored positions */
  def length: Int = positions.length

  /**
    * @return The last position
    */
  def head: Position = positions.head

  /**
    * @return All but the last position.
    */
  def tail: Positions = Positions(positions.tail)
}

object Positions {
  /** @return an empty list of positions */
  def empty: Positions = NoPositions

  /**
    * Creates a [[chess.framework.Positions]] from existing positions
    * and calculates the highest number of repetitions.
    */
  def apply(positions: IndexedSeq[Position]): Positions = {
    var currentMax = 0
    positions foreach { pos: Position =>
      val max = positions.count(pos == _) - 1
      if (max > currentMax) currentMax = max
    }
    Positions(positions, currentMax)
  }

  def apply(positions: IndexedSeq[Position], maxRepetitionCount: Int): Positions = new Positions(positions, maxRepetitionCount)
}

/**
  * Represents an empty list of positions.
  *
  * @author Felix Lehner
  */
object NoPositions extends Positions(Vector(), 0) {
  override def +(pos: Position): Positions = Positions(Vector(pos), 0)

  override def -- : Positions = this
}

/** Stores a position. */
case class Position(pos: BoardMap) extends AnyVal {
  /**
    * Compares this position with another only taking the piece type and its color into account.
    *
    * @param other another position to compare with
    * @return `true` if equal otherwise `false`
    */
  def ==(other: Position): Boolean = {
    val compared: IndexedSeq[Boolean] =
      for {
        col <- 1 to 8
        column = columnLetter(col)
        row <- 1 to 8
        square = Square(column, row)
        otherPiece = other.pos(square)
        piece = this.pos(square)
      } yield {
        val bool = piece match {
          case p: Pawn => p == otherPiece
          case p: King => p == otherPiece
          case p: Rook => p == otherPiece
          case _ => piece === otherPiece
        }
        bool
      }
    val neg = compared contains false
    !neg
  }
}
