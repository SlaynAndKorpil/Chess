package chess.framework

import chess.framework.ChessBoard.columnLetter

import scala.xml._

/**
  * Saves all former positions of [[chess.framework.ChessBoard]]
  *
  * @param positions a list of known positions
  * @version alpha 0.1
  * @author Felix Lehner
  */
class Positions(val positions: Array[Position], val maxRepetition: Int) {

  /** Adds a new position and updates the repetition counter. */
  def +(that: Position): Positions = {
    val maxRep: Int = {
      val temp: Int = positions count (that == _)
      val len = temp
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
    * @return
    */
  def -- : Positions = Positions(positions.tail)

  /** @return the number of stored positions */
  def length: Int = positions.length

  def head: Position = positions.head

  def tail: Positions = Positions(positions.tail)

  /**
    * Converts this object to xml.
    *
    * @see [[chess.framework.ChessBoard#save]]
    * @see [[chess.framework.SaveLoader]]
    * @return an xml object
    */
  def toXML: NodeSeq = {
    //    seqToNodeSeq(positions.foldLeft(NodeSeq.Empty)((node: Seq[Node], pos: Position) => {
    //      seqToNodeSeq(node.theSeq ++ pos.pos)
    //    }))
      <placeholder for="positions.toXML"/>
  }
}

object Positions {
  /** @return an empty list of positions */
  def empty: Positions = NoPositions

  /**
    * Creates a [[chess.framework.Positions]] from existing positions
    * and calculates the highest number of repetitions.
    */
  def apply(positions: Array[Position]): Positions = {
    var currentMax = 0
    positions foreach { pos: Position =>
      val max = positions.count(pos == _) - 1
      if (max > currentMax) currentMax = max
    }
    Positions(positions, currentMax)
  }

  def apply(positions: Array[Position], maxRepetitionCount: Int): Positions = new Positions(positions, maxRepetitionCount)
}

/**
  * Represents an empty list of positions.
  *
  * @author Felix Lehner
  */
object NoPositions extends Positions(Array(), 0) {
  override def +(pos: Position): Positions = Positions(Array(pos), 0)

  override def -- : Positions = this

  override def toXML: NodeSeq = NodeSeq.Empty
}

/** Stores a position in XML format. */
case class Position(pos: Map[Char, Column]) extends AnyVal {
  /**
    * Compares this position with another.
    *
    * @note This tests only for things relevant for repetition.
    * @param other another position to compare with
    * @return `true` if equal otherwise `false`
    */
  def ==(other: Position): Boolean = {
    val compared: Seq[Seq[Boolean]] =
      for (x <- 1 to 8; col = columnLetter(x)) yield {
        val otherPos = other.pos(col)
        val thisPos = this.pos(col)
        for (y <- 1 to 8) yield {
          val otherPiece = otherPos(y)
          val piece = thisPos(y)
          piece match {
            case Pawn(_, _) => piece == otherPiece //TODO moved is not the only relevance for en passant
            case King(_, _) => piece == otherPiece
            case Rook(_, _) => piece == otherPiece
            case _ =>
              piece === otherPiece
          }
        }
      }
    val neg = compared.flatten contains false
    !neg
  }
}
