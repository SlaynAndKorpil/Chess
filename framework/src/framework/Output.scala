package framework

/**
  * Output of the `receive` method of [[framework.ChessBoard]].
  * This contains an updated board and a list of events.
 *
  * @author Felix Lehner
  * @version alpha 0.1
  */
case class Output(board: ChessBoard, events: IndexedSeq[IOEvents.IOEvent]) {
  /**
    *  An utility method that is used to wrap this in a [[scala.Some]] because
    *  most often methods will return an optional output.
 *
    *  @see [[framework.ChessBoard#receive]]
    */
  def asSome: Some[Output] = Some(this)
}
