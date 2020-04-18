package framework

/**
  * An interface to the console for more uniform and coincide messages
  */
trait ConsoleOutput {
  /** The type of message */
  val typeDescription: String

  /** Describes the physical origin (i.e. the package) */
  val name: String

  /** Describes the output (i.e. System.out.println) */
  val output: String => Unit

  /**
    * Formats [[name]], [[typeDescription]] and the message to a uniform console output
    * and then puts it in the [[output]].
    */
  def write(message: String): Unit = output(s"$typeDescription $name >> $message")
}
