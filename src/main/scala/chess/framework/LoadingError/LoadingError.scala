package chess.framework.LoadingError

/** Represents an error that occurred when loading a chess board. */
trait LoadingError {
  val description: String
}

case class FileNotFoundError(path: String) extends LoadingError {
  override val description: String = s"Unable to load file $path"
}

object UnknownVersionError extends LoadingError {
  override val description: String = "The file is either corrupted or from a newer version."
}

object ParsingError extends LoadingError {
  override val description: String = "Failed to parse this file."
}

case class ColumnLoadingError(columnXML: String) extends LoadingError {
  override val description: String = "Failed to load a column from xml: " + columnXML
}

case class PieceLoadingError(pieceXML: String) extends LoadingError {
  override val description: String = "Failed to load a piece from xml: " + pieceXML
}

case class BoardLoadingError(squaresXML: String) extends LoadingError {
  override val description: String = "Failed to load a board from xml: " + squaresXML
}

case class HistoryError(history: String) extends LoadingError {
  override val description: String = "Failed to load history from xml: " + history
}

case class GameStatusLoadingError(status: String) extends LoadingError {
  override val description: String = "Failed to load status " + status
}
