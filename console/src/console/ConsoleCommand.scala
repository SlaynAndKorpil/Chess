package console

/**
  * This is a command for the [[console.InputInterpreter]].
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
  val paramInfo: String = InputInterpreter.noParams

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

  /**
    * The name of the command as given in `names`
    * or [[console.InputInterpreter#noName noName]] when no name is given.
    */
  // needs to be lazy as it otherwise throws a NullPointerException
  lazy val name: String =
    names.headOption match {
      case Some(s) => s
      case None => InputInterpreter.noName
    }

  /**
    * The aliases for name.
    * An empty [[scala.Array]] when the `names` array only contains the name.
    */
  // needs to be lazy as it otherwise throws a NullPointerException
  lazy val aliases: Array[String] =
    if (names.length > 1) names.tail
    else Array()
}
