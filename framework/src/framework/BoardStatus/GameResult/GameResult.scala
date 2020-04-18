package framework.BoardStatus.GameResult

import framework.BoardStatus.ResultReason._
import framework._

/**
  * The result of an ended game.
  * @author Felix Lehner
  * @version alpha 0.2
  */
sealed trait GameResult {
  /** The reason of this result. */
  val reason: ResultReason#Value
}

object GameResult {
  def apply(result: String): Option[GameResult] = {
    val last = result.length - 1
    if (contains(result, "BlackWins(", ")"))
      getResultReason(result.substring(10, last), WinResultReason) flatMap (x => Some(BlackWins by x))
    else if (contains(result, "WhiteWins(", ")"))
      getResultReason(result.substring(10, last), WinResultReason) flatMap (x => Some(WhiteWins by x))
    else if (contains(result, "Draw(", ")")) {
      getResultReason(result.substring(5, last), DrawResultReason) flatMap (x => Some(Draw by x))
    } else None
  }

  private def getResultReason[T <: Enumeration with ResultReason](reason: String, enum: T): Option[T#Value] =
    if (reason forall (_.isDigit)) {
      val id = reason.toInt
      if (enum.maxId <= id) Some(enum(id))
      else None
    } else if (enum.values.exists(_.toString == reason))
      Some(enum.withName(reason))
    else None

  private[BoardStatus] def contains(in: String, start: String, end: String): Boolean =
    in.startsWith(start) && in.endsWith(end)
}

sealed trait Win extends GameResult {
  val reason: WinResultReason.Value
}

object Win {
  def apply(winner: AnyColor): WinResultReason.Value => Win = winner match {
    case White => WhiteWins.apply
    case Black => BlackWins.apply
  }
}

final case class BlackWins(reason: WinResultReason.Value) extends Win

object BlackWins {
  def by(reason: WinResultReason.Value): BlackWins = BlackWins(reason)
}

final case class WhiteWins(reason: WinResultReason.Value) extends Win

object WhiteWins {
  def by(reason: WinResultReason.Value): WhiteWins = WhiteWins(reason)
}

final case class Draw(reason: DrawResultReason.Value) extends GameResult

object Draw {
  def by(reason: DrawResultReason.Value): Draw = Draw(reason)
}
