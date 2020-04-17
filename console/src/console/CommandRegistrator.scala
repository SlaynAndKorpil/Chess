package console

/**
  * A system that stores and organizes [[ConsoleCommand commands]].
  *
  * @author Felix Lehner
  * @version alpha 0.3
  */
trait CommandRegistrator {
  private var registeredCommandsByName: Map[String, ConsoleCommand] = Map()
  private var registeredCommandsSet: Set[ConsoleCommand] = Set()

  /**
    * Adds a command to the other registered commands.
    */
  def registerCommand(newCommand: ConsoleCommand): Unit = {
    registeredCommandsSet += newCommand
    registeredCommandsByName ++= newCommand.names map (_.toLowerCase -> newCommand)
  }

  /**
    * Deletes/ removes a command.
    */
  def unregisterCommand(command: ConsoleCommand): Unit = {
    registeredCommandsSet -= command
    registeredCommandsByName filterKeys (key => !command.hasName(key.toLowerCase))
  }

  /**
    * Gives all name-to-command bindings that are currently registered.
    */
  def allCommandBindings: Map[String, ConsoleCommand] = registeredCommandsByName

  /**
    * All commands registered.
    */
  def allCommands: Set[ConsoleCommand] = registeredCommandsSet

  /**
    * Returns the command with the given name.
    * **WARNING: Does not check if the name does exist! Use [[console.CommandRegistrator#existsCommand existsCommand]]**
    */
  def getCommand(name: String): ConsoleCommand =
    registeredCommandsByName(name.toLowerCase)

  /**
    * Tests if a command name is registered.
    */
  def existsCommand(name: String): Boolean =
    registeredCommandsSet exists (_.hasName(name))
}
