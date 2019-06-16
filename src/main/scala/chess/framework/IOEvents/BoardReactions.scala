package chess.framework.IOEvents

import chess.framework.JavaInterfacing.JReaction
import chess.framework.{Debugger, Error}

/**
  * Contains and provokes reactions
  * to [[chess.framework.IOEvents events]].
  *
  * @author Felix Lehner
  * @version alpha 0.1
  */
class BoardReactions {
  private var reactions: Array[PartialFunction[IOEvent, Unit]] = Array()

  /**
    * Chooses the first fitting response to an event
    * and calls the associated reaction.
    */
  def apply(event: IOEvent): Unit =
    reactions find (_.isDefinedAt(event)) match {
    case Some(eventHandler) => eventHandler(event)
    case None =>
  }

  def +=(reaction: PartialFunction[IOEvent, Unit]): Unit =
    reactions +:= reaction

//  def +=(reaction: Reaction): Unit =
//    reactions +:= {
//      case reaction.isDefinedFor => reaction.reaction(_)
//    }

  def ++=(reactions: IndexedSeq[PartialFunction[IOEvent, Unit]]): Unit =
    this.reactions ++= reactions

  /**
    * Adds a reaction.
    * Used for convenience reasons when implementing a [[chess.framework.ChessIO]] with java.
    */
  def add[T <: IOEvent](reaction: JReaction[T]): Unit = this += new PartialFunction[IOEvent, Unit] {
    override def isDefinedAt(x: IOEvent): Boolean = x match {
      case _: T => true
      case _ => false
    }

    override def apply(v1: IOEvent): Unit = reaction.reaction.accept(v1.asInstanceOf[T])
  }
}
