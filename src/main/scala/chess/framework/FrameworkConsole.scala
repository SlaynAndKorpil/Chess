package chess.framework

import chess.{ConsoleDebugger, ConsoleError}

trait FrameworkConsole {
  self: chess.ConsoleOutput =>
  override val name = "framework"
}

object Debugger extends ConsoleDebugger with FrameworkConsole

object Error extends ConsoleError with FrameworkConsole
