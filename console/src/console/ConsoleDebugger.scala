package console

/**
  * [Description]
  *
  * @author Felix Lehner
  * @version
  */
trait ConsoleDebugger extends ConsoleOutput {
  override val typeDescription: String = "DEBUG"
  def debug (message: String): Unit = println(formatMessage(message))
}
