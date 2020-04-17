package main.chess

/**
  * The main class for this application.
  * Starts the GUI if the input arguments do not
  * contain the string `"console"`. In that case
  * it will start the console interface.
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
object Main extends App {
  println("version 0.3")
  //  new test.test.TestImpl();
  if (args contains "console") console.Run.main(Array())
  else graphics.Run.main(Array())
}
