package chess.framework.GameStatus

trait ResultReason

trait WinResultReason extends ResultReason
object Mate extends WinResultReason
object Resign extends WinResultReason
object Time extends WinResultReason

trait DrawResultReason extends ResultReason
object Stalemate extends DrawResultReason
object DrawAgreement extends DrawResultReason
object Repetition extends DrawResultReason
