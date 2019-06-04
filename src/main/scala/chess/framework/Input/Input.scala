package chess.framework.Input

import chess.framework.{AnyColor, AnyPiece, Square}

/**
  * Possible inputs for ChessBoard
  */
sealed abstract class Input[InputType] (val value: InputType)

case class MoveParams(from: Square, to: Square) extends Input(from -> to)
case class Promotion(piece: (AnyColor, Boolean) => AnyPiece) extends Input(piece)

sealed trait Command extends Input[Unit]

object Resign extends Command
object DrawOffer extends Command
object DrawReject extends Command
object DrawAcceptance extends Command
object TakebackAcceptance extends Command
object TakebackReject extends Command
object TakebackProposal extends Command
