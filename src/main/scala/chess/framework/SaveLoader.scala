package chess.framework

import chess.framework.BoardStatus.GameStatus.GameStatus
import chess.framework.ChessBoard.columnLetter
import chess.framework.LoadingError._

import scala.language.postfixOps
import scala.xml._

object SaveLoader {
  /** the most recent loader */
  val preferredLoader: Loader = loaderForVersion(ChessBoard.Version)

  /**
    * Matches the input's version with known versions to find the correct way of handling the data.
    *
    * @param xml input toXml data
    * @return a [[ChessBoard]] or [[None]] as a failure
    */
  def load(xml: Elem)(implicit io: ChessIO): Either[LoadingError, ChessBoard] = {
    val version =
      try { (xml \@ "version").toLong }
      catch {
        case _: Throwable => -1
      }
    val loader = loaderForVersion(version)
    loader.load(xml)
  }

  /**
    * This method chooses depending on the version which loader should be used.
    *
    * @note when adding a new data format (i.e. a new loader and version id) the loader must be added here.
    * @see [[chess.framework.ChessBoard.Version]]
    * @param version the version id
    * @return a loader
    */
  def loaderForVersion(version: Long): Loader = version match {
    case 1 => Loader2
    case 0 => Loader1
    case -1 => NoLoaderDefined //reservation for fallback loader
    case _ => NoLoaderDefined
  }

  def extractWithFilter(xml: Node, nodeName: String): String =
    (xml \ nodeName).text.filter(c => c != ' ' && c != '\n')

  trait Loader {
    def loadSquaresFromXML(xml: Node): Either[LoadingError, IndexedSeq[(Char, Column)]]

    /** Loads a board from toXml */
    def load(xml: Elem)(implicit io: ChessIO): Either[LoadingError, ChessBoard]

    def loadColumnFromXML(xml: NodeSeq): Either[LoadingError, Column]

    def loadPieceFromXML(xml: NodeSeq): Either[LoadingError, Piece]
  }

  /**
    * Chosen when no other loader is defined for a specific version
    */
  object NoLoaderDefined extends Loader {
    override def loadSquaresFromXML(xml: Node): Either[LoadingError, IndexedSeq[(Char, Column)]] = Left(BoardLoadingError(xml.toString))

    override def load(xml: Elem)(implicit io: ChessIO): Either[LoadingError, ChessBoard] = Left(UnknownVersionError)

    override def loadColumnFromXML(xml: NodeSeq): Either[LoadingError, Column] = Left(ColumnLoadingError(xml.toString))

    override def loadPieceFromXML(xml: NodeSeq): Either[LoadingError, Piece] = Left(PieceLoadingError(xml.toString))
  }

  //TODO rewrite this chaos
  object Loader1 extends Loader {
    override def load(xml: Elem)(implicit io: ChessIO): Either[LoadingError, ChessBoard] = {
      val boardData = xml \ "board"
      val moves = xml \ "moves" \ "move"
      val color = Color(extractWithFilter(xml, "turn"))
      color match {
        case col: AnyColor =>
          val squares = loadSquaresFromXML(boardData.head)

          val history =
            try Right(
              moves map (move =>
                MoveData(
                  Square(extractWithFilter(move, "start").head, extractWithFilter(move, "start").last.asDigit),
                  Piece(extractWithFilter(move, "movedPiece").head, Color(extractWithFilter(move, "movedPiece").last), moved = true),
                  Square(extractWithFilter(move, "end").head, extractWithFilter(move, "end").last.asDigit),
                  extractWithFilter(move, "capture").toBoolean
                )) toList
            )
            catch {
              case _: Throwable => Left(HistoryError(moves.toString))
            }

          val positions: Either[LoadingError, Positions] = {
            val pos: Seq[Either[LoadingError, IndexedSeq[(Char, Column)]]] = (xml \ "positions" \ "pos") map loadSquaresFromXML
            if (pos.isEmpty) Right(Positions.empty)
            else {
              val errors = pos.filter(_.isLeft)
              if (errors.nonEmpty) errors.head.asInstanceOf[Left[LoadingError, Positions]]
              else {
                val positions = pos map (_.right.get) map (_.toMap) map (p => Position(p))
                Right(Positions(positions.toArray))
              }
            }
          }

          val color = col

          val gameStatus = GameStatus(extractWithFilter(xml, "boardStatus"))

          if (squares.isLeft) Left(squares.left.get)
          else if (history.isLeft) Left(history.left.get)
          else if (positions.isLeft) Left(positions.left.get)
          else {
            Right(new ChessBoard(squares = squares.right.get.toMap, history = history.right.get, positions = positions.right.get, turn = color, gameStatus = gameStatus.right.get))
          }
        case _ => Left(ParsingError)
      }
    }

    override def loadSquaresFromXML(xml: Node): Either[LoadingError, IndexedSeq[(Char, Column)]] = {
      val loadedSquares: IndexedSeq[(Char, Either[LoadingError, Column])] = for {
        x <- 1 to 8
        col = columnLetter(x)
      } yield col -> loadColumnFromXML(xml \ col.toString.toUpperCase)
      val possibleError = loadedSquares find (_._2.isLeft) map (_._2.left)
      possibleError match {
        case None =>
          val res = loadedSquares.map { tup =>
            (tup._1, tup._2.right.get)
          }
          Right(res)
        case Some(error) =>
          Left(error.get)
      }
    }

    /**
      * Loads a [[chess.framework.Column]] from toXml data.
      *
      * @param xml data formatted as toXml
      * @return the loaded column
      */
    override def loadColumnFromXML(xml: NodeSeq): Either[LoadingError, Column] = {
      try {
        var pieces: Array[Piece] = Array.empty
        var errors: Array[LoadingError] = Array.empty
        val loadedData =
          for {
            i <- 1 to 8
            label = "l" + i
            data = xml \ label
          } yield if (data.isEmpty) Right(NoPiece) else loadPieceFromXML(data)
        loadedData foreach {
          case Right(piece) =>
            pieces :+= piece
          case Left(error) =>
            errors :+= error
        }
        if (pieces.length >= 8) Right(new Column(pieces))
        else Left(errors.head)
      }
      catch {
        case _: Throwable => Left(ColumnLoadingError(xml.toString))
      }
    }


    override def loadPieceFromXML(xml: NodeSeq): Either[LoadingError, Piece] = {
      if (xml.isEmpty || xml.head.isEmpty) Left(PieceLoadingError(xml.toString))
      else {
        val data = xml.head
        Right(Piece(extractWithFilter(data, "id").head, Color(extractWithFilter(data, "color")), extractWithFilter(data, "moved").toBoolean))
      }
    }
  }

  object Loader2 extends Loader {
    override def load(xml: Elem)(implicit io: ChessIO): Either[LoadingError, ChessBoard] = try {
      val boardData = xml \ "board"
      val moves = xml \ "moves" \ "move"
      val color = Color(extractWithFilter(xml, "turn"))
      color match {
        case col: AnyColor =>
          val squares = loadSquaresFromXML(boardData.head)

          val history =
            try Right(
              moves map (move => {
                val start = move \@ "start"
                val movedPiece = (move \@ "piece").drop(1)
                val end = move \@ "end"
                val captured = move \@ "capture"
                MoveData(
                  Square(start.head, start.last.asDigit),
                  Piece(movedPiece.head, Color(movedPiece.last), moved = true),
                  Square(end.head, end.last.asDigit),
                  captured.toBoolean
                )
              }) toList
            )
            catch {
              case _: Throwable => Left(HistoryError(moves.toString))
            }

          val positions: Either[LoadingError, Positions] = {
            val pos: Seq[Either[LoadingError, IndexedSeq[(Char, Column)]]] = (xml \ "positions" \ "pos") map loadSquaresFromXML
            if (pos.isEmpty) Right(Positions.empty)
            else {
              val errors = pos.filter(_.isLeft)
              if (errors.nonEmpty) errors.head.asInstanceOf[Left[LoadingError, Positions]]
              else {
                val positions = pos map (_.right.get) map (_.toMap) map (p => Position(p))
                Right(Positions(positions.toArray))
              }
            }
          }

          val color = col

          val gameStatus = GameStatus(extractWithFilter(xml, "boardStatus"))

          if (squares.isLeft) Left(squares.left.get)
          else if (history.isLeft) Left(history.left.get)
          else if (positions.isLeft) Left(positions.left.get)
          else if (gameStatus.isLeft) Left(gameStatus.left.get)
          else {
            Right(new ChessBoard(squares = squares.right.get.toMap, history = history.right.get, positions = positions.right.get, turn = color, gameStatus = gameStatus.right.get))
          }
        case _ => Left(ParsingError)
      }
    }
    catch {
      case e: Throwable =>
        e.printStackTrace()
        Left(ParsingError)
    }

    override def loadSquaresFromXML(xml: Node): Either[LoadingError, IndexedSeq[(Char, Column)]] = {
      val loadedSquares: IndexedSeq[(Char, Either[LoadingError, Column])] = for {
        x <- 1 to 8
        col = columnLetter(x)
      } yield col -> loadColumnFromXML(xml \ col.toString.toUpperCase)
      val possibleError = loadedSquares find (_._2.isLeft) map (_._2.left)
      possibleError match {
        case None =>
          val res = loadedSquares.map { tup =>
            (tup._1, tup._2.right.get)
          }
          Right(res)
        case Some(error) =>
          Left(error.get)
      }
    }

    /**
      * Loads a [[chess.framework.Column]] from toXml data.
      *
      * @param xml data formatted as toXml
      * @return the loaded column
      */
    override def loadColumnFromXML(xml: NodeSeq): Either[LoadingError, Column] = {
      try {
        var pieces: Array[Piece] = Array.empty
        var errors: Array[LoadingError] = Array.empty
        val loadedData =
          for {
            i <- 1 to 8
            label = "l" + i
            data = xml \ label
          } yield if (data.isEmpty) Right(NoPiece) else loadPieceFromXML(data)
        loadedData foreach {
          case Right(piece) =>
            pieces :+= piece
          case Left(error) =>
            errors :+= error
        }
        if (pieces.length >= 8) Right(new Column(pieces))
        else Left(errors.head)
      }
      catch {
        case _: Throwable => Left(ColumnLoadingError(xml.toString))
      }
    }


    override def loadPieceFromXML(xml: NodeSeq): Either[LoadingError, Piece] = {
      try {
        if (xml.isEmpty || xml.head.isEmpty) Left(PieceLoadingError(xml.toString))
        else {
          val data = xml.head
          Right(Piece((data \@ "id").head, Color(data \@ "color"), (data \@ "moved").toBoolean))
        }
      } catch {
        case _: Throwable => Left(PieceLoadingError(xml.toString))
      }
    }
  }

}
