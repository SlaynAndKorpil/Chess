package framework

trait ConsoleError extends ConsoleOutput {
  override val typeDescription: String = "ERROR"
  override val output: String => Unit = System.err.println
}
