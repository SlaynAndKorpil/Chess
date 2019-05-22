package chess.framework

import scala.xml._

/**
  * Saves all former positions of [[chess.framework.ChessBoard]]
  *
  * @param pos a list of known positions
  * @version alpha 0.1
  * @author Felix Lehner
  */
case class Positions(pos: Array[Position] = Array()) {
  private val positions: Set[Position] = pos.toSet

  /** Adds a new position */
  def +(pos: Position): Positions = {
    positions.find(_.pos == pos.pos) match {
      case Some(x) => Positions(((positions - x) + Position(pos.pos, pos.weight+1)).toArray)
      case None => Positions((positions + pos).toArray)
    }
  }

  /**
    * Converts this object to xml.
    *
    * @see [[chess.framework.ChessBoard#save]]
    * @see [[chess.framework.SaveLoader]]
    * @return
    */
  import scala.xml.NodeSeq.seqToNodeSeq
  def toXML: NodeSeq = seqToNodeSeq(positions.foldLeft(NodeSeq.Empty) ((node: Seq[Node], pos: Position) => {
    seqToNodeSeq(node.theSeq ++ <pos>{pos.pos}</pos>)
  }))
}

case class Position(pos: String, weight: Int = 0)
