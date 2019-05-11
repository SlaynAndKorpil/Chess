package chess.console

import chess.{ConsoleError, ConsoleDebugger}

sealed trait ConsoleConsole {
  self: chess.ConsoleOutput =>
  override val name = "console-interface"
}

object Debugger extends ConsoleDebugger with ConsoleConsole

object Error extends ConsoleError with ConsoleConsole
