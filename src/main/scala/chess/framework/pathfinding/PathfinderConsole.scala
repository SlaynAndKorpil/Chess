package chess.framework.pathfinding

import chess.{ConsoleDebugger, ConsoleError}

trait PathfinderConsole extends chess.ConsoleOutput {
  override val name = "pathfinding"
}

object Debugger extends ConsoleDebugger with PathfinderConsole

object Error extends ConsoleError with PathfinderConsole
