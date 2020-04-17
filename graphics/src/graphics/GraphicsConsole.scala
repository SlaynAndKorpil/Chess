package graphics

import framework.{ConsoleDebugger, ConsoleError, ConsoleOutput}

trait GraphicsConsole extends ConsoleOutput {
  override val name: String = "graphics"
}

object Debugger extends ConsoleDebugger with GraphicsConsole

object Error extends ConsoleError with GraphicsConsole
