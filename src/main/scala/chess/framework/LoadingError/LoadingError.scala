package chess.framework.LoadingError

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
  override val description: String = "Unable to parse this file."
}

case class ColumnLoadingError(columnXML: String) extends LoadingError {
  override val description: String = "Unable to load a column from xml: " + columnXML
}

case class PieceLoadingError(pieceXML: String) extends LoadingError {
  override val description: String = "Unable to load a piece from xml: " + pieceXML
}

case class BoardLoadingError(squaresXML: String) extends LoadingError {
  override val description: String = "Unable to load a board from xml: " + squaresXML
}

case class HistoryError(history: String) extends LoadingError {
  override val description: String = "Unable to load history from xml: " + history
}
