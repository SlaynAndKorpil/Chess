package chess.console

import chess.framework._
import ChessBoard._
import chess.framework.BoardStatus.GameResult.GameResult
import chess.framework.Input._

import scala.language.postfixOps

/**
  * An interface between the console and the board
  *
  * @param plainBoard a board to play on
  */
class InputInterpreter (plainBoard: ChessIO => ChessBoard) extends ChessIO {
  private[chess] override var board: ChessBoard = plainBoard(this)

  /**
    * runs an interpretation of the console and plays the interpreted moves on the [[board]]
    */
  def run(): Unit = {
    println(board)
    gameLoop()

    def gameLoop(): Unit = {
      println(board.turn.toString.toUpperCase)
      val input = scala.io.StdIn.readLine

      if (input.length >= 4) {
        if (isValidNotation(input)) {
          move(SquareCoordinate(input(0), input(1) asDigit), SquareCoordinate(input(2), input(3) asDigit))
          println(board)
        }
        else {
          val inputs = input.splitAt(input.indexOf(' '))
          val in1 = inputs._1
          val in2 = inputs._2.tail
          in1 match {
            case "resign" =>
              board.receive(Resign)
            case "stop" =>
              println("stopping process...")
              System.exit(0)
            case "save" =>
              println("saving...")
              save(board, in2)
            case "load" => load(in2) match {
              case Some(loadedBoard) =>
                board = loadedBoard
                println(board)
              case None => Error error "This save file is either from an older version or not existing."
            }
            case _ => Error error "wrong input format"
          }
        }
      }
      else Error error "wrong input format"
      gameLoop()
    }

    def isValidNotation (s: String): Boolean =
      s(0).isLetter && s(1).isValidInt && s(2).isLetter && s(3).isValidInt && SquareCoordinate(s(0), s(1).asDigit).isValid && SquareCoordinate(s(2), s(3).asDigit).isValid
  }

  def move (from: SquareCoordinate, to: SquareCoordinate): Unit = receiveInput(MoveParams(from, to))

  override def showDrawOffer(): Unit = {}

  override def removeDrawOffer(): Unit = {}

  override def showPromotion(): Unit = ()

  override def removePromotion(): Unit = ()

  override def showTakeback(): Unit = ()

  override def removeTakeback(): Unit = ()

  override def showEnded(result: GameResult): Unit = Debugger debug s"ended with $result"

  override def update(): Unit = println(board)

  override def showCheck(on: SquareCoordinate): Unit = ()
}
