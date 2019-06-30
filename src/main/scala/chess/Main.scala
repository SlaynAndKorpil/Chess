package chess

object Main extends App {
    if (args contains "console") chess.console.Run.main(Array())
    else chess.graphics.Run.main(Array())
}
