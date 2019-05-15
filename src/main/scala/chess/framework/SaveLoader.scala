package chess.framework

import chess.framework.ChessBoard.columnLetter
import chess.framework.GameStatus._

import scala.language.postfixOps
import scala.xml._

/**
  * A loader for in `.save` files saved chessboards.
  */
class SaveLoader {
  private def extractWithFilter(xml: Node, nodeName: String): String =
    (xml \ nodeName).text.filter(c => c != ' ' && c != '\n')

  /**
    * Matches the input's version with known versions to find the correct way of handling the data.
    *
    * @param xml input xml data
    * @return a [[ChessBoard]] or [[None]] as a failure
    */
  def load(xml: Elem): Option[ChessIO => ChessBoard] = {
    val version = (xml \@ "version").toLong
    val loader = version match {
      case 0 =>
        Debugger debug s"Loading file with loader for version 0"
        Loader1
      case _ =>
        Error error s"unknown save version: $version"
        NoLoaderDefined
    }
    loader.load(xml)
  }

  private trait Loader {
    def load(xml: Elem): Option[ChessIO => ChessBoard]
  }

  private object NoLoaderDefined extends Loader {
    override def load(xml: Elem): Option[ChessIO => ChessBoard] = None
  }

  private object Loader1 extends Loader {
    override def load(xml: Elem): Option[ChessIO => ChessBoard] = {
      val boardData = xml \ "board"
      val moves = xml \ "moves" \ "move"
      val color = Color(extractWithFilter(xml, "turn"))
      color match {
        case col: AnyColor => Some(io => new ChessBoard(
          (for (x <- 1 to 8; col = columnLetter(x).toString.toUpperCase) yield columnLetter(x) -> Column.loadFromXML(boardData \ col)).toMap,
          moves map (move =>
            MoveData(
              SquareCoordinate(extractWithFilter(move, "start").head, extractWithFilter(move, "start").last.toInt),
              Piece(extractWithFilter(move, "movedPiece").head, Color(extractWithFilter(move, "movedPiece").last), moved = true),
              SquareCoordinate(extractWithFilter(move, "end").head, extractWithFilter(move, "end").last.toInt),
              extractWithFilter(move, "capture").toBoolean
            )) toList,
          col,
          io,
          GameStatus.GameStatus(extractWithFilter(xml, "boardStatus")) match {
            case Left(status) => status
            case Right(message) =>
              Error error s"$message"
              StandardReq
          }
        ))
        case _ => None
      }
    }
  }

}
