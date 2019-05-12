package chess.framework

import chess.framework.GameStatus._
import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.xml.Elem

/**
  * Defines a classical 1 vs 1 chess board.
  * Use the companion object to initialize.
  */
class ChessBoard(
                  val squares: Map[Char, Column],
                  override val history: List[MoveData],
                  val turn: Color = White,
                  val io: ChessIO,
                  override val gameStatus: GameStatus
                ) extends BoardMeta {

  import ChessBoard._

  /**
    * @note When the coordinate is outside the board, [[NoPiece]] is returned.
    * @param sqr coordinates on the board
    * @return the piece at some specified position
    */
  def apply(sqr: SquareCoordinate): Piece = getSquare(sqr) match {
    case Some(x) => x
    case None => NoPiece
  }

  /**
    * @param sqr coordinates of the returned piece
    * @return the chess square at a specific position on the board
    */
  def getSquare(sqr: SquareCoordinate): Option[Piece] =
    if (sqr.isValid) Some(squares(sqr._1)(sqr._2)) else None

  /**
    * @param column the one-letter identifier of a column of squares on the board
    * @return a column of the board
    */
  def getColumn(column: Char): Option[Column] =
    if (isValidColumn(column)) Some(squares(column)) else None


  /**
    * empties a square
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: SquareCoordinate): ChessBoard =
    if (sqr.isValid) updated(sqr, NoPiece)
    else this

  /**
    * Handles different input types
    *
    * @param input some input
    * @return probably a tuple with the new board and the new req object
    */
  def receive[T](input: Input[T]): Option[ChessBoard] = input match {
    case MoveParams(from, to) if gameStatus == StandardReq =>
      move(from, to)
    case Promotion(piece) if gameStatus.isInstanceOf[PromoReq] =>
      io.removePromotion()
      Some(updated(/*FIXME read coordinate from gameStatus*/ SquareCoordinate('a', 1), piece))
    case DrawOffer if gameStatus == StandardReq =>
      io.showDrawOffer()
      Some(new ChessBoard(squares, history, turn, io, DrawAcceptanceReq))
    case DrawReject if gameStatus == DrawAcceptanceReq =>
      //TODO test for repetition
      io.removeDrawOffer()
      Some(new ChessBoard(squares, history, turn, io, StandardReq))
    case DrawAcceptance if gameStatus == DrawAcceptanceReq =>
      //TODO test for repetition
      io.removeDrawOffer()
      Some(new ChessBoard(squares, history, turn, io, Ended(Draw(DrawAgreement))))
    case Resignation if gameStatus == StandardReq =>
      io.showResign()
      Some(resign())
    case _ => None
  }


  /**
    * Moves a piece after testing for validity of the move which depends on the following aspects:
    * both squares have to be on the board, either you capture an enemy piece or you move on an empty square,
    * the moved piece must be of the actual players' color, it must be a legal movement of the type of piece and the player must not be checked after the move.
    * When the move is legal, the pieces and the turn are changed and the move is added to the [[history]].
    *
    * @param from the start square
    * @param to   the end-coordinates
    * @return the (when successful, changed) board
    */
  private def move(from: SquareCoordinate, to: SquareCoordinate): Option[ChessBoard] = {
    val movingPiece = apply(from)
    val endPiece = apply(to)
    val startColor = movingPiece.color
    val endColor = endPiece.color

    def isValid: Boolean =
      from.isValid &&
        to.isValid &&
        from != to &&
        startColor == turn &&
        startColor != endColor &&
        isLegalMove(from, to, movingPiece, endPiece) &&
        !doMove.isCheck(turn)

    def doMove: ChessBoard = {
      val nxtStatus: GameStatus = movingPiece match {
        case Pawn(color, _) =>
          color match {
            case White if to.row == 8 =>
              io.showPromotion()
              PromoReq(to)
            case Black if to.row == 1 =>
              io.showPromotion()
              PromoReq(to)
            case _ => gameStatus
          }
        case _ => gameStatus
      }

      var result: ChessBoard = new ChessBoard(squares, MoveData(from, movingPiece, to, startColor.opposite == endColor) :: history, turn.opposite, io, nxtStatus)

      movingPiece match {
        case Pawn(_, _) if {
          apply(to).isEmpty && from.column != to.column
        } =>
          result = result.emptySquare(SquareCoordinate(to.column, from.row))
        case King(color, _) =>
          if (to._1 == 'c' && color == White) result = result.updated(SquareCoordinate('d', 1), Rook(White)).emptySquare(SquareCoordinate('a', 1))
          else if (to._1 == 'g' && color == White) result = result.updated(SquareCoordinate('f', 1), Rook(White)).emptySquare(SquareCoordinate('h', 1))
          else if (to._1 == 'c' && color == Black) result = result.updated(SquareCoordinate('d', 8), Rook(Black)).emptySquare(SquareCoordinate('a', 8))
          else if (to._1 == 'g' && color == Black) result = result.updated(SquareCoordinate('f', 8), Rook(Black)).emptySquare(SquareCoordinate('h', 8))
        case _ =>
      }

      if (!movingPiece.moved) movingPiece.moved = true
      result.updated(to, movingPiece).emptySquare(from)
    }

    if (isValid) Some(doMove)
    else None
    /*
    TODO tests for...
    TODO - mate
    TODO - stalemate
    TODO - draw by insufficient material; see: http://www.e4ec.org/immr.html
    TODO - draw by repetition; see: https://www.chessprogramming.org/Repetitions
    => I/O interaction
     */
  }

  /**
    * Tests if at least one of the kings is checked by finding their squares and testing these for being attacked.
    *
    * @param color kings of this color are tested
    * @return `true` if the player is checked, otherwise `false`
    */
  def isCheck(color: Color = turn): Boolean =
    (for (c <- 1 to 8; row <- 1 to 8;
          sqr = SquareCoordinate(columnLetter(c), row)) yield {
      val piece = apply(sqr)
      if (piece == King(color) || piece == King(color, moved = true)) isAttacked(sqr)
      else false
    }) contains true

  /**
    * Tests if this is a correct move under consideration of the moved piece's type.
    *
    * @note When capturing en passant the endPiece should be an empty square.
    * @param start      the start square
    * @param end        the ending square
    * @param startPiece the moved piece
    * @param endPiece   the captured piece or [[NoPiece]] for an empty square
    */
  def isLegalMove(start: SquareCoordinate, end: SquareCoordinate, startPiece: Piece, endPiece: Piece): Boolean = {
    val startCIndex = start.colIndx
    val endCIndex = end.colIndx
    val columnDif = endCIndex - startCIndex
    val lineDif = end._2 - start._2

    startPiece match {
      case Pawn(color, moved) =>
        val direction = if (color == White) 1 else -1
        if (!endPiece.isEmpty) (columnDif == 1 || columnDif == -1) && lineDif == direction
        else (columnDif == 0 && lineDif == direction) ||
          !moved && columnDif == 0 && (lineDif == direction || (lineDif == 2 * direction && apply(start + (0, direction)).isEmpty)) ||
          //en passant
          history.nonEmpty && (history.head match {
            case MoveData(sPos, piece, ePos, _) =>
              piece == Pawn(color.opposite, moved = true) &&
                sPos == SquareCoordinate(end.column, ClassicalValues.pawnStartLine(color.opposite)) &&
                ePos == sPos + (0, -2 * direction) &&
                lineDif == direction &&
                (columnDif == 1 || columnDif == -1) &&
                end == ePos + (0, direction)
          })
      case Bishop(_, _) =>
        isEmptyDiagonal(start, end)
      case Knight(_, _) =>
        columnDif * columnDif + lineDif * lineDif == 5
      case Rook(_, _) =>
        isEmptyOrthogonal(start, end)
      case Queen(_, _) =>
        isEmptyOrthogonal(start, end) || isEmptyDiagonal(start, end)
      case King(color, _) =>
        (columnDif <= 1 && columnDif >= -1 && lineDif <= 1 && lineDif >= -1) ||
          //        castle
          (!startPiece.moved && (end._1 == 'c' || end._1 == 'g') && {
            val rookSquare = if (startCIndex < endCIndex) AbstractSqrCoordinate.sqr2indxSqr(end) + (1, 0) else AbstractSqrCoordinate.sqr2indxSqr(end) - (2, 0)
            val rook = apply(rookSquare)
            rook == Rook(color) && !(start to SquareCoordinate(if (startCIndex < endCIndex) 'g' else 'c', start.row)).forall(isAttacked) && isEmptyOrthogonal(start, end)
          })
      case NoPiece => false
    }
  }


  def isAttacked(sqr: SquareCoordinate): Boolean = isAttacked(sqr, apply(sqr).color)

  def isAttacked(sqr: SquareCoordinate, attacked: Color): Boolean = {
    implicit class Intersectable[P <: Piece](val content: Array[P]) {
      def ^[OtherP <: Piece](other: Array[OtherP]): Boolean = (for (i <- content; j <- other) yield j == i) contains true
    }

    //    FIXME still false positive results with queen directly orthogonal to king

    val opponent = attacked.opposite
    val colI = sqr.colIndx
    val row = sqr.row

    def partNxtPiece(inc: (Int, Int)): Piece = nextPiece(NumericSquareCoordinate(colI, sqr.row), NumericSquareCoordinate(inc))

    def partApp(inc: (Int, Int)) = apply(NumericSquareCoordinate(colI, row) + inc)


    val knight: Array[Knight] = Array(Knight(opponent, moved = true), Knight(opponent))
    val king: Array[King] = Array(King(opponent), King(opponent, moved = true))
    val queen: Array[Queen] = Array(Queen(opponent, moved = true), Queen(opponent))
    val bishop: Array[Bishop] = Array(Bishop(opponent, moved = true), Bishop(opponent))
    val rook: Array[Rook] = Array(Rook(opponent, moved = true), Rook(opponent))

    def attackedByKnight: Boolean =
      knight ^ Array(partApp(1, 2), partApp(2, 1), partApp(2, -1), partApp(1, -2), partApp(-1, 2), partApp(-2, 1), partApp(-2, -1), partApp(-1, -2))

    def attackedByKing: Boolean =
      king ^ Array.apply(partApp(1, 1), partApp(-1, 1), partApp(1, -1), partApp(-1, -1), partApp(0, 0), partApp(-1, 0), partApp(1, 0), partApp(0, -1), partApp(0, 1))

    def attackedDiagonally: Boolean =
      queen ++ bishop ^ Array(partNxtPiece(1, 1), partNxtPiece(-1, -1), partNxtPiece(1, -1), partNxtPiece(-1, 1))

    def attackedOrthogonally: Boolean =
      queen ++ rook ^ Array(partNxtPiece(1, 0), partNxtPiece(-1, 0), partNxtPiece(0, -1), partNxtPiece(1, 0))

    //    FIXME pawn check not correct evaluated
    def attackedByPawn: Boolean =
      if (opponent == Black) apply(NumericSquareCoordinate(colI, row) + (1, 1)) == Pawn(opponent) || apply(NumericSquareCoordinate(colI, row) + (-1, 1)) == Pawn(opponent)
      else apply(NumericSquareCoordinate(colI, row) + (1, -1)) == Pawn(opponent) || apply(NumericSquareCoordinate(colI, row) + (-1, -1)) == Pawn(opponent)

    attackedByKnight || attackedByPawn || attackedByKing || attackedDiagonally || attackedOrthogonally
  }

  /**
    * Searches for the next piece; useful for searching on an diagonal or orthogonal
    *
    * @note Returns [[NoPiece]] when no piece is found.
    * @param start     the start square
    * @param increment the incrementation every iteration
    * @return the first piece on a line
    */
  @tailrec
  private def nextPiece(start: NumericSquareCoordinate, increment: NumericSquareCoordinate): Piece = {
    val incremented = start + increment
    if (incremented.isValid) apply(incremented) match {
      case NoPiece => nextPiece(incremented, increment)
      case piece => piece
    }
    else NoPiece
  }

  private def isEmptyOrthogonal(from: SquareCoordinate, to: SquareCoordinate): Boolean = {
    val incremented: SquareCoordinate = AbstractSqrCoordinate.indxSqr2sqr(NumericSquareCoordinate((to.colIndx - from.colIndx).signum, (to._2 - from._2).signum) + from)
    incremented == to ||
      (apply(incremented) match {
        case NoPiece =>
          isEmptyOrthogonal(incremented, to)
        case _ => false
      }) && (from._1 == to._1 || from._2 == to._2)
  }

  /**
    * Tests if two squares are located on the same diagonal and the next piece on this diagonal is the piece on the end-square.
    */
  private def isEmptyDiagonal(from: SquareCoordinate, to: SquareCoordinate): Boolean = {
    val startColIndex = from.colIndx
    val endColIndex = to.colIndx
    val incremented: NumericSquareCoordinate = NumericSquareCoordinate((endColIndex - startColIndex).signum, (to._2 - from._2).signum) + from
    incremented == NumericSquareCoordinate(0, 0) || AbstractSqrCoordinate.indxSqr2sqr(incremented) == to ||
      ((apply(incremented) match {
        case NoPiece => isEmptyDiagonal(incremented, to)
        case _ => false
      }) && (startColIndex - endColIndex == from._2 - to._2 || startColIndex - to._2 == endColIndex - from._2))
  }

  def resign(): ChessBoard = new ChessBoard(squares, history, turn, io, Ended(Win(turn.opposite)(GameStatus.Resign)))

  /**
    * Sets a piece at a specific position.
    */
  def updated(square: SquareCoordinate, piece: Char, color: Color, moved: Boolean): ChessBoard = updated(square, Piece(piece)(color, moved))

  def updated(square: SquareCoordinate, piece: Piece): ChessBoard = {
    val updated = squares(square._1).updated(square._2, piece)
    new ChessBoard(squares.updated(square._1, updated), history, turn, io, gameStatus)
  }


  def save: Elem =
  //TODO save gameStatus
    <chessboard version={Version.toString}>
      <board>
        {for (x <- 1 to 8; col = columnLetter(x)) yield
        <col>
          {squares(col).saveData}
        </col> copy (label = col.toUpper toString)}
      </board>
      <moves>
        {history map (m =>
        <move>
          <start>
            {m.startPos.column}{m.startPos.row}
          </start>
          <end>
            {m.endPos.column}{m.endPos.row}
          </end>
          <movedPiece>
            {m.piece}
          </movedPiece>
          <capture>
            {m.captured}
          </capture>
        </move>)}
      </moves>
      <turn>
        {turn}
      </turn>
    </chessboard>

  /**
    * @see [[chess.console.InputInterpreter]]
    * @return a formatted representation of the board
    */
  override def toString: String = {
    val separationLine: String = "  +---+---+---+---+---+---+---+---+\n"
    (for (x <- 1 to 8; c = columnLetter(x)) yield c + " " + squares(c)) mkString("    1   2   3   4   5   6   7   8\n" + separationLine, "\n" + separationLine, "\n" + separationLine)
  }
}


object ChessBoard {
  /**
    * The data-version; used to verify `.save` files (stored games/ boards).
    *
    * @note this constant will be updated with every update changing the way of saving [[ChessBoard]]s.
    */
  val Version = 0L

  /**
    * Standard values for a classical chess game.
    */
  object ClassicalValues {
    def pawnStartLine(color: Color): Int = color match {
      case White => 2
      case Black => 7
      case NoColor => -1
    }
  }


  /**
    * @return An empty chess board
    */
  def empty(io: ChessIO): ChessBoard = fill(NoPiece, io)

  /**
    * Defines the classical chess standard board
    *
    * @return a chess board with the standard start position
    */
  def classicalBoard(io: ChessIO): ChessBoard = new ChessBoard(
    Map(
      'a' -> new Column(Map(
        1 -> Rook(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Rook(Black)
      )),
      'b' -> new Column(Map(
        1 -> Knight(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Knight(Black)
      )),
      'c' -> new Column(Map(
        1 -> Bishop(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Bishop(Black)
      )),
      'd' -> new Column(Map(
        1 -> Queen(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Queen(Black)
      )),
      'e' -> new Column(Map(
        1 -> King(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> King(Black)
      )),
      'f' -> new Column(Map(
        1 -> Bishop(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Bishop(Black)
      )),
      'g' -> new Column(Map(
        1 -> Knight(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Knight(Black)
      )),
      'h' -> new Column(Map(
        1 -> Rook(White),
        2 -> Pawn(White),
        7 -> Pawn(Black),
        8 -> Rook(Black)
      ))
    ), Nil, White, io, StandardReq
  )

  /**
    * Fills a 8 * 8 board with a specific piece.
    *
    * @return a fully filled [[ChessBoard]]
    */
  def fill(piece: Piece, io: ChessIO): ChessBoard = this (Array.fill(8)(Column(piece)), Nil, White, io).get

  def apply(columns: Array[Column], history: List[MoveData] = Nil, turn: Color = White, io: ChessIO): Option[ChessBoard] =
    if (columns.length >= 8)
      Some(new ChessBoard(
        Map(
          'a' -> columns(0),
          'b' -> columns(1),
          'c' -> columns(2),
          'd' -> columns(3),
          'e' -> columns(4),
          'f' -> columns(5),
          'g' -> columns(6),
          'h' -> columns(7)
        ), history, turn, io, StandardReq
      ))
    else None

  /**
    * Saves a [[ChessBoard]] to a file using the xml format.
    *
    * @param fileName a name for the save; technically it's possible to use a file path but this does often lead to errors (due to a lazy developer...)
    */
  def save(board: ChessBoard, fileName: String = "save"): Unit = {
    val boardData = board.save
    val name = fileName + (if (fileName contains ".") "" else ".save")
    try xml.XML.save(name, boardData)
    catch {
      case _: Throwable => Error error "failed to save the data!"
    }
  }

  /**
    * Loads a [[ChessBoard]] from a file.
    *
    * @param path the file
    * @return a board or [[None]] when the board was saved in a different version.
    */
  def load(path: String): Option[ChessIO => ChessBoard] = {
    val fullPath = path + (if (path contains ".") "" else ".save")
    try new SaveLoader().load(xml.XML.load(fullPath))
    catch {
      case _: Throwable =>
        Error error s"failed to load the file $fullPath"
        None
    }
  }

  def isValidColumn(column: Char): Boolean = columnIndex(column) <= 8 && columnIndex(column) >= 1

  /**
    * Matches a one-letter identifier of a column of the board to its corresponding index.
    */
  def columnIndex(column: Char): Int = 1 + (column match {
    case 'a' => 0
    case 'A' => 0
    case 'b' => 1
    case 'B' => 1
    case 'c' => 2
    case 'C' => 2
    case 'd' => 3
    case 'D' => 3
    case 'e' => 4
    case 'E' => 4
    case 'f' => 5
    case 'F' => 5
    case 'g' => 6
    case 'G' => 6
    case 'h' => 7
    case 'H' => 7
    case _ => -1
  })

  /**
    * Converts a column index to it's corresponding character.
    */
  def columnLetter(column: Int): Char = column match {
    case 1 => 'a'
    case 2 => 'b'
    case 3 => 'c'
    case 4 => 'd'
    case 5 => 'e'
    case 6 => 'f'
    case 7 => 'g'
    case 8 => 'h'
    case _ => ' '
  }
}
