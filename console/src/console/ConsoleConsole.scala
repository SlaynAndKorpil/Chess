package console

sealed trait ConsoleConsole extends ConsoleOutput {
  override val name = "console-interface"
}

object Debugger extends ConsoleDebugger with ConsoleConsole

object Error extends ConsoleError with ConsoleConsole
