package framework

trait ConsoleDebugger extends ConsoleOutput {
  override val typeDescription: String = "DEBUG"
  override val output: String => Unit = println
}
