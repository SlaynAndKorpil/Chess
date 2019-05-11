package chess.graphics

import chess.{ConsoleDebugger, ConsoleError}

trait GraphicsConsole {
  self: chess.ConsoleOutput =>
  override val name: String = "graphics"
}

object Debugger extends ConsoleDebugger with GraphicsConsole

object Error extends ConsoleError with GraphicsConsole
