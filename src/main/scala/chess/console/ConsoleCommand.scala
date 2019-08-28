package chess.console

/**
  * This is a command for the [[chess.console.InputInterpreter]].
  *
  * @author Felix Lehner
  * @version alpha 0.3
  */
trait ConsoleCommand extends (String => CommandResult) {
  /**
    * A full, in-depth description of this command that will be displayed by the "help [this command name]" command.
    */
  val help: String

  /**
    * A short description of this command that will be displayed by the general help command.
    */
  val description: String

  /**
    * All names that should refer to this command.
    */
  val names: Array[String]

  /**
    * This gives information about the expected parameters that can be used to generate more detailed help texts.
    */
  val paramInfo: String

  /**
    * Tests if this command has a certain name.
    */
  def hasName(name: String): Boolean = {
    val lowerCaseName = name.toLowerCase
    names exists (_.toLowerCase == lowerCaseName)
  }

  /**
    * This applies the command to a [[String]] containing parameters.
    */
  override def apply(params: String): CommandResult
}
