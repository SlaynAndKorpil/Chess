package chess.console

import chess.framework.{ChessBoard, ChessIO}

object Run extends App {
  val board = (io: ChessIO) => ChessBoard.classicalBoard(io)

  val interpreter = new InputInterpreter(board)
  interpreter.run()
}
