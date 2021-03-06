package framework.IOEvents

import framework.javaInterfacing.Reactions.JReaction

/**
  * Contains and provokes reactions
  * to [[framework.IOEvents events]].
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
class BoardReactionHandler {
  private var reactions = Vector.empty[PartialFunction[IOEvent, Unit]]

  /**
    * Chooses the first fitting response to an event
    * and calls the associated reaction.
    */
  def apply(event: IOEvent): Unit =
    reactions find (_.isDefinedAt(event)) match {
    case Some(eventHandler) => eventHandler(event)
    case None =>
  }

  /** Adds a reaction for specific events (defined with `isDefinedAt` method of reaction.) */
  def +=(reaction: PartialFunction[IOEvent, Unit]): Unit =
    reactions +:= reaction

  /** Adds a list of reactions. */
  def ++=(reactions: IndexedSeq[PartialFunction[IOEvent, Unit]]): Unit =
    this.reactions ++= reactions

  /**
    * Creates a reaction from a [[framework.javaInterfacing.Reactions.JReaction JReaction]]
    * and adds it the the reaction list.
    * Used for convenience reasons when implementing a [[framework.ChessIO]] with java.
    */
  def add[T <: IOEvent](reaction: JReaction[T]): Unit = this += new PartialFunction[IOEvent, Unit] {
    override def isDefinedAt(event: IOEvent): Boolean = reaction.isDefinedAt(event)

    override def apply(event: IOEvent): Unit = reaction.reaction.accept(event.asInstanceOf[T])
  }
}
