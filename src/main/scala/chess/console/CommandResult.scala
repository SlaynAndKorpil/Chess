package chess.console

import chess.framework.Input.Input

/**
  * A result returned by an executed command.
  *
  * @author Felix Lehner
  * @version alpha 0.3
  */
sealed trait CommandResult {
  /**
    * A message that normally will get printed into the console.
    */
  val message: String
}

/**
  * Just a boring text message.
  */
case class Message(override val message: String) extends CommandResult

/**
  * A question offering inputs for a chessboard depending on the answer.
  * @param message the question
  * @param approval an input for the case of acceptance
  * @param reject an input for the case of a negative answer
  */
case class Question(override val message: String, approval: Input[_], reject: Input[_]) extends CommandResult

/**
  * A message that expects the app to quit.
  */
case class Quit(override val message: String) extends CommandResult
