package chess.framework.Input

import chess.framework.{AnyColor, AnyPiece, Square}

/**
  * Inputs for [[chess.framework.ChessBoard#receive ChessBoard]] or [[chess.framework.ChessIO#receiveInput ChessIO]]
  *
  * @version alpha 0.1
  * @author Felix Lehner
  */
sealed abstract class Input[InputType](val value: InputType)

/** A move action. */
final case class MoveParams(from: Square, to: Square) extends Input(from -> to)

/** A promotion to a piece type. */
final case class Promotion(piece: (AnyColor, Boolean) => AnyPiece) extends Input(piece)

sealed trait Command extends Input[Unit]

/** Resignation of the player that is to move. */
object Resign extends Command

/** Draw offer. */
object DrawOffer extends Command

/** Declining a offered draw. */
object DrawReject extends Command

/** Acceptance of a proposed draw. */
object DrawAcceptance extends Command

/** Accepting a proposed takeback. */
object TakebackAcceptance extends Command

/** Rejecting a takeback. */
object TakebackReject extends Command

/** Asking for a takeback. */
object TakebackProposal extends Command
