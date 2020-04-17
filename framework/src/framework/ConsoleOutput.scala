package framework

/**
  * An interface to the console for more uniform and coincide messages
  */
trait ConsoleOutput {
  /**The type of message*/
  val typeDescription: String

  /**Describes the physical origin (i.e. the package)*/
  val name: String

  /**
    * Formats [[name]], [[typeDescription]] and the message to a uniform console output.
    */
  def formatMessage (message: String): String = s"$typeDescription $name >> $message"
}
