package framework.BoardStatus.ResultReason

/**
  * The reason why a game ended with a specific result.
  *
  * @version alpha 0.2
  * @author Felix Lehner
  */
sealed trait ResultReason extends Enumeration

/** Reasons for a win. */
object WinResultReason extends ResultReason {
  val Mate: WinResultReason.Value = Value
  val Resignation: WinResultReason.Value = Value
  val Time: WinResultReason.Value = Value
}

/** Reasons for a draw. */
object DrawResultReason extends ResultReason {
  val Stalemate: DrawResultReason.Value = Value
  val DrawAgreement: DrawResultReason.Value = Value
  val Repetition: DrawResultReason.Value = Value
  val Blocked: DrawResultReason.Value = Value
  val InsufficientMaterial: DrawResultReason.Value = Value
  val FiftyMovesRule: DrawResultReason.Value = Value
}
