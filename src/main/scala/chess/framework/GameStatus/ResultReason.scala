package chess.framework.GameStatus

trait ResultReason {
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
    case _ => None
  }
}

trait WinResultReason extends ResultReason

object Mate extends WinResultReason {
  override val toString: String = "Mate"
}

object Resignation extends WinResultReason {
  override val toString: String = "Resignation"
}

object Time extends WinResultReason {
  override val toString: String = "Time"
}

trait DrawResultReason extends ResultReason

object Stalemate extends DrawResultReason {
  override val toString: String = "Stalemate"
}

object DrawAgreement extends DrawResultReason {
  override val toString: String = "DrawAgreement"
}

object Repetition extends DrawResultReason {
  override val toString: String = "Repetition"
}

//TODO add to loading
object Blocked extends DrawResultReason {
  override val toString: String = "Blocked"
}

object InsufficientMaterial extends DrawResultReason {
  override val toString: String = "InsufficientMaterial"
}
