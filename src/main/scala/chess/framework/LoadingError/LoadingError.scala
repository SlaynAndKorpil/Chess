package chess.framework.LoadingError

/**
  * Represents an error that occurred when loading a chess board.
  * As it is a `java.lang.Throwable` you can also use this in a
  * convenient way in a java environment.
  *
  * @author Felix Lehner
  * @version alpha 0.2
  */
trait LoadingError extends RuntimeException {
  override def getMessage: String = description

  val description: String
}

final case class FileNotFoundError(path: String) extends LoadingError {
  override val description: String = s"Unable to load file $path"
}

object UnknownVersionError extends LoadingError {
  override val description: String = "The file is either corrupted or from a newer version."
}

object ParsingError extends LoadingError {
  override val description: String = "Failed to parse this file."
}

final case class ColumnLoadingError(columnXML: String) extends LoadingError {
  override val description: String = "Failed to load a column from xml: " + columnXML
}

final case class PieceLoadingError(pieceXML: String) extends LoadingError {
  override val description: String = "Failed to load a piece from xml: " + pieceXML
}

final case class BoardLoadingError(squaresXML: String) extends LoadingError {
  override val description: String = "Failed to load a board from xml: " + squaresXML
}

final case class HistoryError(history: String) extends LoadingError {
  override val description: String = "Failed to load history from xml: " + history
}

final case class GameStatusLoadingError(status: String) extends LoadingError {
  override val description: String = "Failed to load status " + status
}
