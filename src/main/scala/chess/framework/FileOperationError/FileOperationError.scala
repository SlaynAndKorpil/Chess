package chess.framework.FileOperationError

/**
  * Represents an error that occurred when loading a chess board.
  * As it is a `java.lang.Throwable` you can also use this in a
  * convenient way in a java environment.
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
abstract class FileOperationError extends java.lang.RuntimeException { // java does not like this to be a trait :( :/
  @inline
  override def getMessage: String = description

  val description: String
}

final case class FileNotFoundError(path: String) extends FileOperationError {
  override val description: String = s"Unable to load file $path"
}

object UnknownVersionError extends FileOperationError {
  override val description: String = "The file is either corrupted or from a newer version."
}

trait ParsingError extends FileOperationError

object ParsingError extends ParsingError {
  override val description: String = "Failed to parse this file."
}

final case class ColumnLoadingError(columnXML: String) extends ParsingError {
  override val description: String = "Failed to load a column from xml: " + columnXML
}

final case class PieceLoadingError(pieceXML: String) extends ParsingError {
  override val description: String = "Failed to load a piece from xml: " + pieceXML
}

final case class BoardLoadingError(squaresXML: String) extends ParsingError {
  override val description: String = "Failed to load a board from xml: " + squaresXML
}

final case class HistoryLoadingError(history: String) extends ParsingError {
  override val description: String = "Failed to load history from xml: " + history
}

final case class GameStatusLoadingError(status: String) extends ParsingError {
  override val description: String = "Failed to load status " + status
}
