package chess.framework

import chess.framework.ChessBoard.columnLetter
import chess.framework.GameStatus._

import scala.language.postfixOps
import scala.xml._

/**
  * A loader for in `.save` files saved chessboards.
  */
class SaveLoader {

  import SaveLoader._

  /**
    * Matches the input's version with known versions to find the correct way of handling the data.
    *
    * @param xml input xml data
    * @return a [[ChessBoard]] or [[None]] as a failure
    */
  def load(xml: Elem): Option[ChessIO => ChessBoard] = {
    val version = (xml \@ "version").toLong
    val loader = loaderForVersion(version)
    loader.load(xml)
  }
}

object SaveLoader {
  /** @return the most recent loader */
  val preferredLoader: Loader = loaderForVersion(ChessBoard.Version)

  def extractWithFilter(xml: Node, nodeName: String): String =
    (xml \ nodeName).text.filter(c => c != ' ' && c != '\n')

  /**
    * This method chooses depending on the version which loader should be used.
    *
    * @note when adding a new data format (i.e. a new loader and version id) the loader must be added here.
    * @see [[chess.framework.ChessBoard.Version]]
    * @param version the version id
    * @return a loader
    */
  def loaderForVersion(version: Long): Loader = version match {
    case 0 => Loader1
    case _ => NoLoaderDefined
  }

  trait Loader {
    def loadSquaresFromXML(xml: Node): Seq[(Char, Column)]

    /** Loads a board from xml */
    def load(xml: Elem): Option[ChessIO => ChessBoard]
  }

  /**
    * Chosen when no other loader is defined for this version
    */
  object NoLoaderDefined extends Loader {
    /** @return always returns [[scala.collection.immutable.Seq#empty]] */
    override def loadSquaresFromXML(xml: Node): Seq[(Char, Column)] = Seq.empty

    /** @return always returns [[scala.None]] because there is no loading operation known for this version */
    override def load(xml: Elem): Option[ChessIO => ChessBoard] = None
  }

  object Loader1 extends Loader {
    override def load(xml: Elem): Option[ChessIO => ChessBoard] = {
      val boardData = xml \ "board"
      val moves = xml \ "moves" \ "move"
      val color = Color(extractWithFilter(xml, "turn"))
      color match {
        case col: AnyColor =>
          val squares = loadSquaresFromXML(boardData.head).toMap

          val history = moves map (move =>
            MoveData(
              SquareCoordinate(extractWithFilter(move, "start").head, extractWithFilter(move, "start").last.toInt),
              Piece(extractWithFilter(move, "movedPiece").head, Color(extractWithFilter(move, "movedPiece").last), moved = true),
              SquareCoordinate(extractWithFilter(move, "end").head, extractWithFilter(move, "end").last.toInt),
              extractWithFilter(move, "capture").toBoolean
            )) toList

          val positions: Positions = {
            val pos = xml \ "positions" \ "pos"
            if (pos.isEmpty) Positions.empty
            else (pos foldRight Positions.empty) ((x: NodeSeq, y: Positions) => y + Position(x.head))
          }

          val color = col

          val gameStatus = GameStatus.GameStatus(extractWithFilter(xml, "boardStatus")) match {
            case Left(status) => status
            case Right(message) =>
              Error error s"$message"
              StandardReq
          }

          Some(io => new ChessBoard(squares, history, positions, color, io, gameStatus))
        case _ => None
      }
    }

    override def loadSquaresFromXML(xml: Node): Seq[(Char, Column)] = {
      for (x <- 1 to 8; col = columnLetter(x))
        yield {
          val colXML = xml \ col.toString.toUpperCase
          col -> Column.loadFromXML(colXML)
        }
    }
  }

}
