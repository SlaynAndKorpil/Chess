package framework

trait FrameworkConsole extends ConsoleOutput {
  override val name = "framework"
}

object Debugger extends ConsoleDebugger with FrameworkConsole

object Error extends ConsoleError with FrameworkConsole
