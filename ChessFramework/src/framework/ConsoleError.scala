package framework

trait ConsoleError extends ConsoleOutput {
  override val typeDescription: String = "ERROR"
  def error (message: String): Unit = System.err.println(formatMessage(message))
}
