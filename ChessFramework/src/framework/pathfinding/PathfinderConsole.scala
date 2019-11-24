package framework.pathfinding

import framework.{ConsoleDebugger, ConsoleError, ConsoleOutput}

trait PathfinderConsole extends ConsoleOutput {
  override val name = "pathfinding"
}

object Debugger extends ConsoleDebugger with PathfinderConsole

object Error extends ConsoleError with PathfinderConsole
