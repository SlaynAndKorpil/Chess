package console

object Main extends App {
  val interpreter = new InputInterpreter(args.mkString(" "))
  interpreter.run()
}
