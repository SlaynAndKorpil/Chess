package framework

trait ConsoleDebugger extends ConsoleOutput {
  override val typeDescription: String = "DEBUG"
  def debug (message: String): Unit = println(formatMessage(message))
}
