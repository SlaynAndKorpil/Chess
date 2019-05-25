package chess.framework

import chess.framework.GameStatus._

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.xml.{Elem, NodeSeq}

/**
  * Defines a classical 1 vs 1 chess board.
  * Use the companion object to initialize.
  *
  * @constructor The constructor takes nearly all variables the class consists of
  *              to make immutability possible.
  * @version alpha 0.1
  * @author Felix Lehner
  */
class ChessBoard(
                  val squares: Map[Char, Column],
                  val history: List[MoveData],
                  val positions: Positions,
                  val turn: AnyColor = White,
                  val io: ChessIO,
                  val gameStatus: GameStatus
                ) extends BoardMeta {

  import ChessBoard._

  //TODO find better identifier
  /**
    * The move number
    */
  val turnCounter: Int = history.length / 2

  /**
    * @note When the coordinate is outside the board, [[NoPiece]] is returned.
    * @param sqr coordinates on the board
    * @return the piece at some specified position
    */
  def apply(sqr: SquareCoordinate): Piece = getSquare(sqr) match {
    case Some(piece) => piece
    case None => NoPiece
  }

  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[None]] if the sqr does not exist
    */
  def getSquare(sqr: SquareCoordinate): Option[Piece] =
    if (sqr.isValid) Some(squares(sqr._1)(sqr._2)) else None

  /**
    * @param column the one-letter identifier of a column of squares on the board
    * @return a column of the board, [[None]] if no column with this identifier exists
    */
  def getColumn(column: Char): Option[Column] =
    if (isValidColumn(column)) Some(squares(column)) else None


  /**
    * Empties a square.
    *
    * @param sqr the square to be emptied
    */
  private def emptySquare(sqr: SquareCoordinate): ChessBoard =
    if (sqr.isValid) updated(sqr, NoPiece)
    else this

  /**
    * Handles different input types depending on the [[gameStatus]].
    *
    * @param input some [[Input]]
    * @return an updated [[ChessBoard]] or [[None]] when the input is either unknown
    *         or does not match the current input requirements given by [[gameStatus]].
    */
  def receive[T](input: Input[T]): Option[ChessBoard] = input match {
    case MoveParams(from, to) if gameStatus == StandardReq =>
      move(from, to)

    case Promotion(piece) if gameStatus.isInstanceOf[PromoReq] =>
      promote(piece)

    case DrawOffer if gameStatus == StandardReq =>
      io.showDrawOffer()
      Some(new ChessBoard(squares, history, positions, turn, io, DrawAcceptanceReq))

    case DrawReject if gameStatus == DrawAcceptanceReq =>
      io.removeDrawOffer()
      Some(new ChessBoard(squares, history, positions, turn, io, StandardReq))

    case DrawAcceptance if gameStatus == DrawAcceptanceReq =>
      io.removeDrawOffer()
      Some(clone(gameStatus = Ended(Draw(DrawAgreement))))

    case TakebackProposal if gameStatus == StandardReq =>
      io.showTakeback()
      Some(clone(gameStatus = TakebackAcceptanceReq))

    case TakebackAcceptance if gameStatus == TakebackAcceptanceReq =>
      io.removeTakeback()
      takeback

    case TakebackReject if gameStatus == TakebackAcceptanceReq =>
      io.removeTakeback()
      Some(clone(gameStatus = StandardReq))

    case Resign if gameStatus == StandardReq =>
      io.showResign()
      Some(resign)

    case _ => None
  }


  /**
    * Moves a piece after testing for validity of the move which depends on the following aspects:
    * both squares have to be on the board, either you capture an enemy piece or you move on an empty square,
    * the moved piece must be of the color [[turn]], it must be a legal movement of the type of piece
    * and the player must not be checked after the move.
    * When the move is legal, the pieces and the turn are changed and the move is added to the [[history]].
    *
    * @param from the start square
    * @param to   the end-coordinates
    * @return the updated board
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
        !doMove._1.isCheck(turn)

    def doMove: (ChessBoard, () => Unit) = {
      //TODO rework code
      var action: () => Unit = () => ()
      val nxtStatus: GameStatus = movingPiece match {
        case Pawn(color, _) =>
          color match {
            case White if to.row == 8 =>
              action = () => io.showPromotion()
              PromoReq(to)
            case Black if to.row == 1 =>
              action = () => io.showPromotion()
              PromoReq(to)
            case _ => gameStatus
          }
        case _ => gameStatus
      }

      //adds the currently evaluated move to the history
      val updatedHistory = MoveData(from, movingPiece, to, startColor.opposite == endColor) :: history

      //adds the position to the position history
      val updatedPositions = positions + Position(saveSquares)

      var result: ChessBoard =
        clone(history = updatedHistory, positions = updatedPositions, turn = turn.opposite, gameStatus = nxtStatus)

      //testing for en passant and castling
      movingPiece match {
        case Pawn(_, _) if apply(to).isEmpty && from.column != to.column =>
          result = result.emptySquare(SquareCoordinate(to.column, from.row))
        case King(color, moved)
          if !moved && from == ChessBoard.ClassicalValues.kingStartSquare(turn) &&
            (to.column == 'c' || to.column == 'g') =>
          val row = if (color == White) 1 else 8
          val col = if (to.column == 'c') 'd' else 'f'
          val emptyCol = if (to.column == 'c') 'a' else 'h'
          val res = result.updated(SquareCoordinate(col, row), Rook(color)).emptySquare(SquareCoordinate(emptyCol, row))
          result = res
        case _ =>
      }

      val resPiece = Piece(movingPiece.identifier, movingPiece.color, moved = true)
      result.updated(to, resPiece).emptySquare(from) -> action
    }

    val movedBoard = doMove

    //TODO maybe pass action via class-system back to receive?
    movedBoard._2()

    if (isValid) Some(doMove._1)
    else None
  }

  /**
    * Tests if at least one of the kings is checked by finding their squares and testing these for being attacked.
    *
    * @param color kings of this color are tested
    * @return `true` if the player is checked, otherwise `false`
    */
  private def isCheck(color: AnyColor = turn): Boolean = {
    val attackedKings = for (c <- 1 to 8; row <- 1 to 8;
                             sqr = SquareCoordinate(columnLetter(c), row)) yield {
      val piece = apply(sqr)
      if (piece == King(color) || piece == King(color, moved = true)) isAttacked(sqr)
      else false
    }
    attackedKings contains true
  }

  /**
    * Tests if this is a correct move under consideration of the moved piece's type.
    *
    * @note When capturing en passant the endPiece should be an empty square.
    * @param start      the start square
    * @param end        the ending square
    * @param startPiece the moved piece
    * @param endPiece   the captured piece or [[NoPiece]] for an empty square
    * @return `true` if the move is legal, otherwise `false`
    */
  private def isLegalMove(start: SquareCoordinate, end: SquareCoordinate, startPiece: Piece, endPiece: Piece): Boolean = {
    val startCIndex = start.colIndx
    val endCIndex = end.colIndx
    val columnDif = endCIndex - startCIndex
    val lineDif = end._2 - start._2

    startPiece match {
      case Pawn(color, moved) =>
        val direction = ClassicalValues.pawnDir(color)
        if (!endPiece.isEmpty) (columnDif == 1 || columnDif == -1) && lineDif == direction
        else (columnDif == 0 &&
          (lineDif == direction || (!moved && lineDif == 2 * direction && apply(start + (0, direction)).isEmpty))) ||
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
          //castle
          (!startPiece.moved && (end._1 == 'c' || end._1 == 'g') && {
            val rookSquare =
              if (startCIndex < endCIndex) AbstractSqrCoordinate.sqr2indxSqr(end) + (1, 0)
              else AbstractSqrCoordinate.sqr2indxSqr(end) - (2, 0)
            val rook = apply(rookSquare)
            val squaresToTest = start to SquareCoordinate(if (startCIndex < endCIndex) 'g' else 'c', start.row)

            def isSqrAttacked(sqr: AbstractSqrCoordinate[_]): Boolean = sqr match {
              case square: SquareCoordinate => isAttacked(square, turn)
              case square: NumericSquareCoordinate => isAttacked(square, turn)
            }

            rook == Rook(color) && squaresToTest.forall(sqr => !isSqrAttacked(sqr)) && isEmptyOrthogonal(start, end)
          })
      case NoPiece => false
    }
  }


  /**
    * An overloading method. It takes the color of the piece on the square as
    * the ''defender'', i.e. the not-attacking color.
    *
    * @see isAttacked(sqr: SquareCoordinate, attacked: AnyColor): Boolean
    * @return `true` if the square is attacked, otherwise `false`
    */
  private def isAttacked(sqr: SquareCoordinate): Boolean = {
    val attackedCol = apply(sqr).color
    attackedCol match {
      case col: AnyColor => isAttacked(sqr, col)
      case NoColor => false
    }
  }

  /**
    * Tests a square for being attacked.
    *
    * @param sqr      the square that gets tested
    * @param attacked the ''defending'' color
    * @return `true` if the square is attacked, otherwise `false`
    */
  private def isAttacked(sqr: SquareCoordinate, attacked: AnyColor): Boolean = {
    implicit class Intersectable[P <: Piece](val content: Array[P]) {
      def ^[OtherP <: Piece](other: Array[OtherP]): Boolean = (for (i <- content; j <- other) yield j == i) contains true
    }

    val opponent = attacked.opposite
    val colI = sqr.colIndx
    val row = sqr.row

    def partNxtPiece(inc: (Int, Int)): Piece = nextPiece(NumericSquareCoordinate(colI, sqr.row), NumericSquareCoordinate(inc))

    def partApply(inc: (Int, Int)) = apply(NumericSquareCoordinate(colI, row) + inc)


    val knight: Array[Knight] = Array(Knight(opponent, moved = true), Knight(opponent))
    val king: Array[King] = Array(King(opponent), King(opponent, moved = true))
    val queen: Array[Queen] = Array(Queen(opponent, moved = true), Queen(opponent))
    val bishop: Array[Bishop] = Array(Bishop(opponent, moved = true), Bishop(opponent))
    val rook: Array[Rook] = Array(Rook(opponent, moved = true), Rook(opponent))
    val pawn: Array[Pawn] = Array(Pawn(opponent, moved = true), Pawn(opponent))

    def attackedByKnight: Boolean =
      knight ^ Array(partApply(1, 2), partApply(2, 1), partApply(2, -1), partApply(1, -2), partApply(-1, 2), partApply(-2, 1), partApply(-2, -1), partApply(-1, -2))

    def attackedByKing: Boolean =
      king ^ Array.apply(partApply(1, 1), partApply(-1, 1), partApply(1, -1), partApply(-1, -1), partApply(0, 0), partApply(-1, 0), partApply(1, 0), partApply(0, -1), partApply(0, 1))

    def attackedDiagonally: Boolean =
      queen ++ bishop ^ Array(partNxtPiece(1, 1), partNxtPiece(-1, -1), partNxtPiece(1, -1), partNxtPiece(-1, 1))

    def attackedOrthogonally: Boolean =
      queen ++ rook ^ Array(partNxtPiece(1, 0), partNxtPiece(-1, 0), partNxtPiece(0, -1), partNxtPiece(0, 1))

    def attackedByPawn: Boolean = {
      val dir = ClassicalValues.pawnDir(opponent.opposite)

      def pieceAtOffset(colD: Int, rowD: Int): Piece = apply(NumericSquareCoordinate(colI, row) + (colD, rowD))

      pawn ^ Array(pieceAtOffset(1, dir), pieceAtOffset(-1, dir))
    }

    attackedByKnight || attackedByPawn || attackedByKing || attackedDiagonally || attackedOrthogonally
  }

  /**
    * Searches for the next piece.
    *
    * @usecase useful for searching on an diagonal or orthogonal
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

  /**
    * Tests if two squares are on the same orthogonal and if the space between them is empty.
    *
    * @see isEmptyDiagonal
    */
  private def isEmptyOrthogonal(from: SquareCoordinate, to: SquareCoordinate): Boolean = {
    val incremented: SquareCoordinate =
      NumericSquareCoordinate((to.colIndx - from.colIndx).signum, (to._2 - from._2).signum) + from
    incremented == to ||
      (apply(incremented) match {
        case NoPiece =>
          isEmptyOrthogonal(incremented, to)
        case _ => false
      }) && (from._1 == to._1 || from._2 == to._2)
  }

  /**
    * Tests if two squares are located on the same diagonal and the next piece on this diagonal is the piece on the end-square.
    *
    * @see isEmptyOrthogonal
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

  /**
    * Updates the board.
    *
    * @param square the coordinate of the square to updated
    * @param piece  the piece the square shall be updated to
    * @return a ChessBoard with updated squares.
    */
  private[framework] def updated(square: SquareCoordinate, piece: Piece): ChessBoard = {
    val updated = squares(square._1).updated(square._2, piece)
    clone(squares = squares.updated(square._1, updated))
  }

  /**
    * Generates a new [[ChessBoard]] which shares all attributes with this one
    * but all that are defined in the parameters.
    *
    * @usecase Used to change specific values of the board.
    * @return the new board
    */
  def clone(
             squares: Map[Char, Column] = this.squares,
             history: List[MoveData] = this.history,
             positions: Positions = this.positions,
             turn: AnyColor = this.turn,
             io: ChessIO = this.io,
             gameStatus: GameStatus = this.gameStatus) =
    new ChessBoard(squares, history, positions, turn, io, gameStatus)

  private def saveSquares: NodeSeq =
    for (x <- 1 to 8; col = columnLetter(x)) yield
      <col>
        {squares(col).saveData}
      </col> copy (label = col.toUpper toString)


  /**
    * Stores the board in the xml format.
    *
    * @note The version attribute is used when loading to find the right loading method.
    * @usecase Used to save&load [[ChessBoard]]s
    * @see [[chess.framework.SaveLoader]]
    * @see [[chess.framework.ChessBoard#save]]
    * @return an xml-[[scala.xml.Elem]] with all data of the [[chess.framework.ChessBoard]]
    */
  def save: Elem =
    <chessboard version={Version.toString}>
      <board>
        {saveSquares}
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
      <positions>
        {positions.toXML}
      </positions>
      <boardStatus>
        {gameStatus}
      </boardStatus>
    </chessboard>

  /**
    * Formats the board as a [[String]]; useful for console output
    *
    * @see [[chess.console.InputInterpreter]]
    * @return a formatted representation of the board
    */
  override def toString: String = {
    val separationLine: String = "  +---+---+---+---+---+---+---+---+\n"
    val lines = for (x <- 1 to 8; c = columnLetter(x)) yield c + " " + squares(c)
    lines mkString("    1   2   3   4   5   6   7   8\n" + separationLine, "\n" + separationLine, "\n" + separationLine)
  }
}


object ChessBoard {
  /**
    * The data-version; used to verify `.save` files (stored games/ boards).
    *
    * @note this constant will be updated with every update changing the way of saving [[ChessBoard]]s.
    * @version alpha 0.1
    */
  val Version = 0L

  /**
    * Standard values for a classical chess game.
    */
  object ClassicalValues {
    /** @return the row where all pawns of this color are placed. */
    def pawnStartLine(color: AnyColor): Int =
      if (color == White) 2 else 7

    /** @return the row where all pieces of this color are generated. */
    def piecesStartLine(color: AnyColor): Int =
      if (color == White) 1 else 8

    /** @return the [[chess.framework.SquareCoordinate]] where the king of this color is placed. */
    def kingStartSquare(color: AnyColor): SquareCoordinate =
      SquareCoordinate('e', piecesStartLine(color))

    /**
      * The moving-direction of pawns of this color (i.e. the line difference they are taking in every normal move)
      *
      * @return `1` for [[chess.framework.White]] and `-1` for [[chess.framework.Black]]
      */
    def pawnDir(color: AnyColor): Int =
      if (color == White) 1 else -1
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
    ), Nil, Positions.empty, White, io, StandardReq
  )

  /**
    * Fills a 8 * 8 board with a specific piece.
    *
    * @return a fully filled [[chess.framework.ChessBoard]]
    */
  def fill(piece: Piece, io: ChessIO): ChessBoard = this (Array.fill(8)(Column(piece)), Nil, Positions.empty, White, io).get

  def apply(columns: Array[Column], history: List[MoveData] = Nil, positions: Positions = Positions.empty, turn: AnyColor = White, io: ChessIO): Option[ChessBoard] =
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
        ), history, positions, turn, io, StandardReq
      ))
    else None

  /**
    * Saves a [[chess.framework.ChessBoard]] to a `.save` file using the xml format.
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
    * Loads a [[chess.framework.ChessBoard]] from a file.
    *
    * @param path the file
    * @return a board or [[scala.None]] when the board was saved in a different version.
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

  /** @return `true` if a column with this identifier does exist, else `false`*/
  def isValidColumn(column: Char): Boolean =
    columnIndex(column) <= 8 && columnIndex(column) >= 1

  /**
    * Matches a one-letter identifier of a column to its corresponding index.
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
