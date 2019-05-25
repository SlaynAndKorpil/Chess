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

  /** Adds a new position */
  def +(pos: Position): Positions = {
    val maxRepetition: Int = {
      val temp = positions filter (_ == pos)
      if (temp.length > this.maxRepetition) temp.length
      else this.maxRepetition
    }
    Positions(positions :+ pos, maxRepetition)
  }

  /**
    * Deletes the last position.
    *
    * @usecase Useful for takebacks
    * @see [[chess.framework.ChessBoard]]
    * @return
    */
  def -- : Positions = Positions(positions.tail)

  def length: Int = positions.length

  /**
    * Converts this object to xml.
    *
    * @see [[chess.framework.ChessBoard#save]]
    * @see [[chess.framework.SaveLoader]]
    * @return an xml object
    */

  import scala.xml.NodeSeq.seqToNodeSeq

  def toXML: NodeSeq = seqToNodeSeq(positions.foldLeft(NodeSeq.Empty)((node: Seq[Node], pos: Position) => {
    //TODO does not work properly yet
    seqToNodeSeq(node.theSeq ++ <pos>
      {pos.pos}
    </pos>)
  }))
}

object Positions {
  def empty: Positions = NoPositions

  def apply(positions: Array[Position], maxRepetitionCount: Int): Positions = new Positions(positions, maxRepetitionCount)

  /** Creates a [[chess.framework.Positions]] from existing positions. */
  def apply(positions: Array[Position]): Positions = {
    val maxRepetition: Int = {
      var currMax = 0
      positions foreach { pos =>
        val max = positions.count(_ == pos)
        if (max > currMax) currMax = max
      }
      currMax
    }
    Positions(positions, maxRepetition)
  }
}

object NoPositions extends Positions(Array(), 0) {
  override def +(pos: Position): Positions = Positions(Array(pos), 1)

  override def -- : Positions = this

  override def toXML: NodeSeq = NodeSeq.Empty
}

/** Stores a position in XML format. */
case class Position(/*FIXME moved attribute of most pieces should be ignored*/ pos: NodeSeq) extends AnyVal {
  /**
    * Compares this position with another.
    * @note This tests only for things relevant for repetition.
    * @param other another position to compare with
    * @return `true` if equal otherwise `false`
    */
  def ==(other: Position): Boolean = {
    val compared: Seq[Seq[Boolean]] = for (x <- 1 to 8; col = columnLetter(x).toString.toUpperCase) yield {
      val otherPos = other.pos \ col
      val thisPos = pos \ col
      for (y <- 1 to 8; row = "l"+y) yield {
        otherPos.theSeq.exists(_.label == row) == thisPos.theSeq.exists(_.label == row) && {
          val otherPiece = Piece.fromXML(otherPos \ row)
          val piece = Piece.fromXML(pos \ row)
          piece match {
            case Pawn(_, _) => piece == otherPiece //TODO moved is not the only relevance for en passant
            case King(_, _) => piece == otherPiece
            case Rook(_, _) => piece == otherPiece
            case _ => piece.color == otherPiece.color && piece.isInstanceOf[otherPiece.type]
          }
        }
      }
    }
    compared.flatten forall (_ == true)
  }
}
