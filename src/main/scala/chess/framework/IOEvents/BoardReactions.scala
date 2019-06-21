package chess.framework.IOEvents

import chess.framework.javaInterfacing.Reactions.JReaction
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
    * Creates a reaction from a [[chess.framework.javaInterfacing.Reactions.JReaction JReaction]]
    * and adds it the the reaction list.
    * Used for convenience reasons when implementing a [[chess.framework.ChessIO]] with java.
    */
  def add[T <: IOEvent](reaction: JReaction[T]): Unit = this += new PartialFunction[IOEvent, Unit] {
    override def isDefinedAt(event: IOEvent): Boolean = reaction.isDefinedAt(event)

    override def apply(event: IOEvent): Unit = reaction.reaction.accept(event.asInstanceOf[T])
  }
}