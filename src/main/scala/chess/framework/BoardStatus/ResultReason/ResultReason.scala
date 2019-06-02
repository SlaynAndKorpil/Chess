package chess.framework.BoardStatus.ResultReason

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

sealed trait WinResultReason extends ResultReason

object Mate extends WinResultReason {
  override val toString: String = "Mate"
}

object Resignation extends WinResultReason {
  override val toString: String = "Resignation"
}

object Time extends WinResultReason {
  override val toString: String = "Time"
}

sealed trait DrawResultReason extends ResultReason

object Stalemate extends DrawResultReason {
  override val toString: String = "Stalemate"
}

object DrawAgreement extends DrawResultReason {
  override val toString: String = "DrawAgreement"
}

object Repetition extends DrawResultReason {
  override val toString: String = "Repetition"
}

object Blocked extends DrawResultReason {
  override val toString: String = "BlockedPosition"
}

object InsufficientMaterial extends DrawResultReason {
  override val toString: String = "InsufficientMaterial"
}
