package chess.framework.BoardStatus.ResultReason

/**
  * The reason why a game ended with a specific result.
  *
  * @version alpha 0.1
  * @author Felix Lehner
  */
sealed trait ResultReason {
  override def toString: String
}

object ResultReason {
  def apply(reason: String): Option[ResultReason] = reason match {
    case Mate.toString => Some(Mate)
    case Resignation.toString => Some(Resignation)
    case Time.toString => Some(Resignation)
    case Stalemate.toString => Some(Resignation)
    case DrawAgreement.toString => Some(DrawAgreement)
    case Repetition.toString => Some(Repetition)
    case Blocked.toString => Some(Blocked)
    case InsufficientMaterial.toString => Some(InsufficientMaterial)
    case _ => None
  }
}

/** Reasons for a win. */
sealed trait WinResultReason extends ResultReason

/** Ended by mate */
object Mate extends WinResultReason {
  override val toString: String = "Mate"
}

/** Ended by resignation */
object Resignation extends WinResultReason {
  override val toString: String = "Resignation"
}

/** Ended by time */
object Time extends WinResultReason {
  override val toString: String = "Time"
}

/** Reasons for a draw. */
sealed trait DrawResultReason extends ResultReason

/** Drawn because of a stalemate */
object Stalemate extends DrawResultReason {
  override val toString: String = "Stalemate"
}

/** Draw because of a draw agreement */
object DrawAgreement extends DrawResultReason {
  override val toString: String = "DrawAgreement"
}

/** Draw because of repetition */
object Repetition extends DrawResultReason {
  override val toString: String = "Repetition"
}

/** Draw as a result of a blocked position. */
object Blocked extends DrawResultReason {
  override val toString: String = "BlockedPosition"
}

/** Draw by insufficient material. */
object InsufficientMaterial extends DrawResultReason {
  override val toString: String = "InsufficientMaterial"
}
