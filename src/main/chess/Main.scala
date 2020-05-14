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

  if (args contains "console") console.Main.main(args
      .filter(arg => arg.toLowerCase != "console" && arg.toLowerCase != "cli"))
  else graphics.Main.main(args)
}
