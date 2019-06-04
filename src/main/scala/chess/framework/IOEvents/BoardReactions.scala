package chess.framework.IOEvents

class BoardReactions {
  private var reactions: Array[PartialFunction[IOEvent, Unit]] = Array()

  def apply(event: IOEvent): Unit =
    reactions find (_.isDefinedAt(event))  match {
    case Some(eventHandler) => eventHandler(event)
    case None =>
  }

  def +=(reaction: PartialFunction[IOEvent, Unit]): Unit =
    reactions +:= reaction

  def ++=(reactions: IndexedSeq[PartialFunction[IOEvent, Unit]]): Unit =
    this.reactions ++= reactions
}
