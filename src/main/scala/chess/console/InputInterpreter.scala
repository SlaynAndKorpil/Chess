package chess.console

import chess.framework._
import ChessBoard.{save, load}
import chess.framework.IOEvents._
import chess.framework.Input._

import scala.language.postfixOps

/**
  * An interface between the console and the board
  */
class InputInterpreter extends ChessIO {
  var board: ChessBoard = ChessBoard.classicalBoard

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
          move(Square(input(0), input(1) asDigit), Square(input(2), input(3) asDigit))
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
              case Some(errorMessage) => Error error errorMessage.description
              case None =>
            }
            case _ => Error error "wrong input format"
          }
        }
      }
      else Error error "wrong input format"
      gameLoop()
    }

    def isValidNotation (s: String): Boolean =
      s(0).isLetter && s(1).isValidInt && s(2).isLetter && s(3).isValidInt && Square(s(0), s(1).asDigit).isValid && Square(s(2), s(3).asDigit).isValid
  }

  def move (from: Square, to: Square): Unit = receiveInput(MoveParams(from, to))

  override def update(): Unit = println(board)

  chessReactions += {
    case ShowEnded(result) => println(s"ended with $result")
    case message => println(message.toString)
  }
}
