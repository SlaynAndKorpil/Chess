package console

import framework.BoardStatus.GameResult._
import framework.BoardStatus.ResultReason.WinResultReason._
import framework.BoardStatus.ResultReason.DrawResultReason._
import framework.BoardStatus.ResultReason.ResultReason
import framework.BoardStatus.GameStatus._
import framework.IOEvents.{ShowEnded, ShowPromotion}
import framework.Input._
import framework._

import scala.language.postfixOps

/**
  * An interface between the console and the board.
  * It interprets command line input as commands.
  *
  * @author Felix Lehner
  * @version alpha 0.3
  */
class InputInterpreter extends ChessIO with CommandRegistrator {
  import InputInterpreter._

  var board: ChessBoard = ChessBoard.classicalBoard

  // registers all commands
  Commands.register()

  // Stores reactions to acceptations/ rejections of questions.
  private var approval: Input[_] = _
  private var reject: Input[_] = _

  /**
    * Runs an interpretation of the console and plays the interpreted moves on the [[InputInterpreter#board board]].
    */
  def run(): Unit = {
    println(board)
    gameLoop()

    @scala.annotation.tailrec
    def gameLoop(): Unit = {
      val input = scala.io.StdIn.readLine

      chessBoard.gameStatus match {
        case DrawAcceptanceReq | TakebackAcceptanceReq =>
          input.toLowerCase match {
            case "y" | "yes" | "yep" => receiveInput(approval)
            case "n" | "no" | "nope" => receiveInput(reject)
            case _ => print("This is not an answer to a yes/no question. Please type either \"y\" or \"n\".")
          }
        case PromoReq(_) =>
          input.toLowerCase match {
            case "q" => receiveInput(Promotion(Queen))
            case "r" => receiveInput(Promotion(Rook))
            case "k" => receiveInput(Promotion(Knight))
            case "b" => receiveInput(Promotion(Bishop))
            case _ => print("This is not a valid piece identifier. Please type on of the following: Q R K B")
          }
        case _ =>
          parseInput(input) match {
            case NoMessage =>
            case Message(message) =>
              println('\n' + message + '\n')
            case Question(message, approval, reject) =>
              this.approval = approval
              this.reject = reject
              print(message)
            case Quit(message) =>
              println(message)
              return
          }
      }

      gameLoop()
    }
  }

  /**
    * This parses the input by either finding the command that corresponds to the first word in the input
    * and applying the rest of the input as parameters to it, interpreting the input as move command
    * or as a response to a question.
    *
    * @param input The input that comes right from the console.
    */
  def parseInput(input: String): CommandResult = {
    import Commands.MoveCommand._

    val words = input.split(" ")
    val commandName = words.head.trim.toLowerCase

    if (input.isEmpty) NoMessage
    else if (existsCommand(commandName)) {
      val command = allCommandBindings(commandName)
      command(words.filterNot(_.isEmpty).tail.mkString(" "))
    }
    else if (isMoveCommand(input)) parseMove(input)
    else Message(commandErrorMessage)
  }

  override def update(): Unit = println(board)

  /**
    * This stores and organizes all commands.
    */
  object Commands {
    /**
      * This contains an array of all commands, so it gets easier to register all at once.
      * When defining new commands, they have to be added here.
      */
    def register(): Unit =
      // registers all commands; all new commands have to be added to this array in order to work properly
      Array(
        LoadCommand,
        SaveCommand,
        TakebackCommand,
        HelpCommand,
        DrawCommand,
        ResignCommand,
        ExitCommand,
        RestartCommand,
        MoveCommand
      ) foreach registerCommand

    object LoadCommand extends ConsoleCommand {
      override val help: String = "Loads a saved game from the given directory."
      override val description: String = "Load a saved game."
      override val names: Array[String] = Array("load")
      override val paramInfo: String = "<file path>"

      override def apply(params: String): Message = load(params) match {
        case Some(error) => Message("ERROR: " + error.description)
        case None => Message("Successfully loaded!")
      }
    }

    object SaveCommand extends ConsoleCommand {
      override val help: String =
        "Saves the current game to a file. When no path is given as a parameter, the last used save directory is chosen."
      override val description: String = "Save this game."
      override val names: Array[String] = Array("save", "s", "write", "w")
      override val paramInfo: String = "<file path>"

      override def apply(params: String): Message = save(params) match {
        case Some(error) => Message("ERROR: " + error.description)
        case None => Message("Successfully saved!")
      }
    }

    object TakebackCommand extends ConsoleCommand {
      override val help: String =
        "Ask your opponent for allowance to take the last move back. Answers can either be y(es) or n(o)."
      override val description: String = "Take the last move back."
      override val names: Array[String] = Array("takeback", "t")
      override val paramInfo: String = noParams

      override def apply(params: String): Question = {
        receiveInput(TakebackProposal)
        Question("Do you want a takeback? (Y/N)", TakebackAcceptance, TakebackReject)
      }
    }

    object DrawCommand extends ConsoleCommand {
      override val help: String =
        "Asks for a draw. When the same position was reached three times or the fifty moves rule applies, a draw by repetition is caused automatically."
      override val description: String = "Propose a draw."
      override val names: Array[String] = Array("draw")
      override val paramInfo: String = noParams

      override def apply(params: String): Question = {
        receiveInput(DrawOffer)
        Question("Do you accept a draw? (Y/N)", DrawAcceptance, DrawReject)
      }
    }

    object ResignCommand extends ConsoleCommand {
      override val help: String = "The currently active color resigns the game."
      override val description: String = "Resign the game."
      override val names: Array[String] = Array("resign")
      override val paramInfo: String = noParams

      override def apply(params: String): Message = {
        receiveInput(Resign)
        Message(chessBoard.turn + " resigned!")
      }
    }

    object RestartCommand extends ConsoleCommand {
      override val help: String = "Restart the game from the beginning."
      override val description: String = "Restart the game."
      override val names: Array[String] = Array("restart")
      override val paramInfo: String = noParams

      override def apply(params: String): CommandResult = {
        chessBoard = ChessBoard.classicalBoard
        Message("Restarted the game.")
      }
    }

    object HelpCommand extends ConsoleCommand {
      override val help: String =
        "Prints either an overview of all available commands or, when a command name is given as parameter, a more detailed and in-depth description of a command."
      override val description: String = "Get help."
      override val names: Array[String] = Array("help", "h", "?", "man", "manual")
      override val paramInfo: String = "[command name]"

      override def apply(params: String): Message = {
        val helpMessage =
          if (existsCommand(params)) specificHelp(getCommand(params))
          else generalHelp
        Message(helpMessage)
      }

      def generalHelp: String = {
        val allHelps = allCommands map (command => {
          val commandNames = command.names.map(_.toUpperCase).mkString(" | ")
          val commandParams = command.paramInfo
          val commandDescription = command.description
          s" - $commandNames $commandParams : $commandDescription"
        })
        allHelps.mkString("===================<HELP>===================\n", "\n", "\n============================================")
      }

      def specificHelp(command: ConsoleCommand): String = {
        val names = command.names map (_.toUpperCase)
        val params =
          if (command.paramInfo == noParams) "NO PARAMETERS"
          else command.paramInfo
        val help = command.help
        s"${names.head}     $params\n" +
          "--------------------------------------------\n" +
          s"$help\n" +
          s"Aliases: ${names mkString "|"}"
      }
    }

    object ExitCommand extends ConsoleCommand {
      override val help: String = "End your life."
      override val description: String = "Stop this BS."
      override val names: Array[String] = Array("exit", "halt", "stop", "die", "end", "shutdown", "quit", "kill")
      override val paramInfo: String = noParams

      override def apply(params: String): Quit = Quit("CYA...")
    }

    object MoveCommand extends ConsoleCommand {
      override val help: String =
        "Try to move from the square defined by the first two characters to the one indicated by the two last ones."
      override val description: String = "Move a piece from one square to another."
      override val names: Array[String] = Array("move", "mv", "m")
      override val paramInfo: String = "<letter><digit><letter><digit>"

      override def apply(params: String): CommandResult =
        if (isMoveCommand(params)) parseMove(params)
        else Message(parameterErrorMessage)

      /**
        * Tests if a [[String]] is a valid move command of the form `letter|digit|letter|digit`.
        */
      def isMoveCommand(command: String): Boolean =
        command.length >= 4 &&
          'a' <= command.head && command.head < 'i' &&
          '0' < command(1) && command(1) < '9' &&
          'a' <= command(2) && command(2) < 'i' &&
          '0' < command(3) && command(3) < '9'

      /**
        * Parses a move command.
        */
      def parseMove(move: String): CommandResult = {
        val from = Sqr(move.head, move(1).asDigit)
        val to = Sqr(move(2), move(3).asDigit)
        val moveInput = MoveParams(from, to)
        receiveInput(moveInput)
        NoMessage
      }
    }

  }

  chessReactions += {
    case ShowEnded(result) =>

      val resultDescription = result match {
        case BlackWins(_) => "Black wins"
        case WhiteWins(_) => "White wins"
        case Draw(_) => "The game is drawn"
      }
      val reason = result.reason match {
        case Mate => "mate"
        case Resignation => "resignation"
        case Time => "timer"
        case Stalemate => "stalemate"
        case DrawAgreement => "draw agreement"
        case Repetition => "repetition"
        case Blocked => "blocked position"
        case InsufficientMaterial => "insufficient material"
        case FiftyMovesRule => "fifty moves rule"
        case rr: ResultReason => rr.toString
      }
      println(s"$resultDescription due to $reason.")
    case ShowPromotion(on) => print(s"To what piece do you want the pawn on $on to promote to? (Q|R|M|B)")
  }
}

object InputInterpreter {
  /**
    * A message that can be printed whenever no correct result can be processed due to wrong input or some other error.
    * It refers to the `help` command to check out the other commands.
    */
  val commandErrorMessage = "Unknown command. Type \"help\" and press 'ENTER' to see all valid commands."

  /**
    * An error message that shows that the used command failed because it expected different parameters.
    */
  val parameterErrorMessage = "Wrong parameters for this option. Try \"help [command name]\"."

  /**
    * When used as parameter description in a command definition
    * this shows that the command does not take any parameters.
    */
  val noParams = ""
}
