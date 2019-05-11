package chess.framework

/**
  * Possible inputs for ChessBoard
  */
sealed abstract class Input[InputType] (val value: InputType)
//TODO add other input possibilities

case class MoveParams(from: SquareCoordinate, to: SquareCoordinate) extends Input(from -> to)
case class Promotion (piece: AnyPiece) extends Input(piece)

trait Command extends Input[Unit]

object Resignation extends Command
object DrawOffer extends Command
object DrawReject extends Command
object DrawAcceptance extends Command
