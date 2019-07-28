package chess.framework

import chess.framework.BoardStatus.GameResult._
import chess.framework.BoardStatus.GameStatus._
import chess.framework.BoardStatus.ResultReason._
import chess.framework.IOEvents._
import chess.framework.Input._
import chess.framework.pathfinding.WaypointResult
import chess.framework.AbstractSqrCoordinate._

import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.xml.{Elem, NodeSeq}

/**
  * Defines a classical 1 vs 1 chess board
  * and defines an access via the `receive` method.
  * Use the companion object to initialize.
  *
  * @constructor The constructor takes nearly all variables the class consists of
  *              to make immutability possible.
  * @version alpha 0.1
  * @author Felix Lehner
  */
case class ChessBoard (
                        squares: Map[Char, Column],
                        history: List[MoveData],
                        positions: Positions,
                        turn: AnyColor,
                        gameStatus: GameStatus
                      )(implicit val io: ChessIO) extends BoardMeta {

  import ChessBoard._

  /** The move counter */
  val turnCounter: Int = history.length / 2

  /** All pieces (excluding [[chess.framework.NoPiece NoPiece]]s) and their position */
  lazy val allPieces: Array[(Square, AnyPiece)] = {
    val allSquares = for {
      x <- 1 to 8
      col = columnLetter(x)
      row <- 1 to 8
      square = Square(col, row)
      piece: Piece = apply(square)
      if piece.nonEmpty
    } yield (square, piece)
    allSquares.toArray.asInstanceOf[Array[(Square, AnyPiece)]]
  }

  /**
    * @param column the one-letter identifier of a column of squares on the board
    * @return a column of the board, [[scala.None]] if no column with this identifier exists
    */
  def getColumn(column: Char): Option[Column] =
    if (isValidColumn(column)) Some(squares(column))
    else None

  /**
    * Handles different input types depending on the [[chess.framework.ChessBoard#gameStatus gameStatus]].
    *
    * @param input some [[chess.framework.Input Input]]
    * @return some [[chess.framework.Output]] or [[scala.None]] when the input is either unknown
    *         or does not match the current input requirements given by [[chess.framework.ChessBoard#gameStatus gameStatus]].
    */
  def receive[T](input: Input[T]): Option[Output] =
    input match {
      case MoveParams(from, to) if gameStatus == StandardReq =>
        move(from, to)

      case Promotion(piece) if gameStatus.isInstanceOf[PromoReq] =>
        promote(piece)

      case DrawOffer if gameStatus == StandardReq =>
        if (positions.maxRepetition >= 3) {
          val res = Draw by Repetition
          Output(clone(gameStatus = Ended(res)), Array(ShowEnded(res))) asSome
        }
        else if (isFiftyMovesRuleApplying) {
          val res = Draw by FiftyMovesRule
          Output(clone(gameStatus = Ended(res)), Array(ShowEnded(res))) asSome
        }
        else Output(clone(gameStatus = DrawAcceptanceReq), Array(ShowDrawOffer)) asSome

      case DrawReject if gameStatus == DrawAcceptanceReq =>
        Output(clone(gameStatus = StandardReq), Array(NoEvent)) asSome

      case DrawAcceptance if gameStatus == DrawAcceptanceReq =>
        val res = Draw(DrawAgreement)
        Output(clone(gameStatus = Ended(res)), Array(ShowEnded(res))) asSome

      case TakebackProposal if gameStatus == StandardReq || gameStatus.isInstanceOf[Ended] =>
        Output(clone(gameStatus = TakebackAcceptanceReq), Array(ShowTakeback)) asSome

      case TakebackAcceptance if gameStatus == TakebackAcceptanceReq =>
        takeback

      case TakebackReject if gameStatus == TakebackAcceptanceReq =>
        Output(clone(gameStatus = StandardReq), Array(RemoveTakeback)) asSome

      case Resign if gameStatus == StandardReq =>
        val res = resign
        Output(clone(gameStatus = res), Array(ShowEnded(res.result))) asSome

      case _ => None
    }

  def mapPiece[T](func: (Square, AnyPiece) => T): IndexedSeq[T] =
    for {
      col <- 1 to 8
      column = columnLetter(col)
      row <- 1 to 8
      square = Square(column, row)
      piece = apply(square)
      if piece.nonEmpty
    } yield func(square, piece.asInstanceOf[AnyPiece])

  /**
    * @note When the coordinate is outside the board, [[chess.framework.NoPiece NoPiece]] is returned.
    * @param sqr coordinates on the board
    * @return the piece at some specified position
    */
  @inline
  def apply(sqr: Square): Piece = getPiece(sqr) getOrElse NoPiece

  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[scala.None]] if the sqr does not exist
    */
  def getPiece(sqr: Square): Option[Piece] =
    if (sqr.isValid) Some(squares(sqr._1)(sqr._2))
    else None

  def filterPieces(func: Piece => Boolean): Map[Char, Column] =
    squares map { tup => tup._1 -> tup._2.filter(func) }

  /**
    * Generates a new [[chess.framework.ChessBoard ChessBoard]] which shares all attributes with this one
    * but all that are defined in the parameters.
    *
    * @usecase Used to change specific values of the board.
    * @return the new board
    */
  @inline
  def clone(
             squares: Map[Char, Column] = this.squares,
             history: List[MoveData] = this.history,
             positions: Positions = this.positions,
             turn: AnyColor = this.turn,
             io: ChessIO = this.io,
             gameStatus: GameStatus = this.gameStatus
           ): ChessBoard = ChessBoard(squares, history, positions, turn, gameStatus)(io)

  /**
    * Stores this object in the xml format.
    *
    * @note The version attribute is used when loading to find the right loading method.
    * @usecase Used to save&load [[chess.framework.ChessBoard ChessBoard]]s
    * @see [[chess.framework.SaveLoader]]
    * @see [[chess.framework.ChessBoard#save]]
    * @return an xml-[[scala.xml.Elem element]] with all relevant data of the [[chess.framework.ChessBoard]]
    */
  def save: Elem =
    <chessboard version={Version.toString}>
      <board>
        {saveSquares(squares)}
      </board>
      <moves>
        {history map (m => <move start={m.startPos.column.toString + m.startPos.row.toString} end={m.endPos.column.toString + m.endPos.row.toString} capture={m.captured.toString} piece={m.piece.toString}/>)}
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
    * Formats the board as a [[String]]
    *
    * @usecase Used in consoleUI to show the board in console
    * @see [[chess.console.InputInterpreter]]
    * @return a formatted representation of the board
    */
  override def toString: String = {
    val separationLine: String = "  +---+---+---+---+---+---+---+---+\n"
    val lines = for (x <- 1 to 8; c = columnLetter(x)) yield c + " " + squares(c)
    lines mkString("    1   2   3   4   5   6   7   8\n" + separationLine, "\n" + separationLine, "\n" + separationLine)
  }

  /**
    * Empties a square.
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: Square): ChessBoard =
    if (sqr.isValid) updated(sqr, NoPiece)
    else this

  /**
    * Moves a piece after testing for validity of the move which depends on the following aspects:
    *
    * -both squares have to be on the board
    * -you must capture an enemy piece or move on an empty square
    * -the moved piece must be of the color [[chess.framework.ChessBoard#turn turn]]
    * -it must be a legal movement of the type of piece
    * -the player must not be checked after the move.
    *
    * When the move is legal, the pieces and the turn are changed and
    * the move is added to the [[chess.framework.ChessBoard#history history]].
    *
    * @param from the start square
    * @param to   the end-coordinates
    * @return the updated board
    */
  def move(from: Square, to: Square): Option[Output] = if (from.isValid && to.isValid) {
    val movingPiece = apply(from)
    val endPiece = apply(to)
    val startColor = movingPiece.color
    val endColor = endPiece.color

    val movedBoard = doMove(from, to, movingPiece, startColor, endColor).clone(positions = positions + Position(squares))

    def isValid: Boolean =
        from != to &&
        startColor == turn &&
        startColor != endColor &&
        isLegalMove(from, to, movingPiece, endPiece) &&
        !movedBoard.isCheck(turn)

    lazy val updatedStatus: GameStatus =
      if (movedBoard isInsufficientMaterial) Ended(Draw by InsufficientMaterial)
      else if (movedBoard.clone(positions = movedBoard.positions + Position(movedBoard.squares)).isFivefoldRepetition)
        Ended(Draw by Repetition)
      else if (movedBoard isStalemate) Ended(Draw by Stalemate)
      else if (movedBoard isMate) Ended(turn match {
        case White => WhiteWins by Mate
        case Black => BlackWins by Mate
      })
      else if (movedBoard isBlocked) Ended(Draw by Blocked)
      else movedBoard.gameStatus


    lazy val endedEvent: IOEvent = updatedStatus match {
      case res: Ended => ShowEnded(res.result)
      case _ => NoEvent
    }

    lazy val promoEvent: IOEvent = movingPiece match {
      case Pawn(color, _) if to.row == ClassicalValues.piecesStartLine(color.opposite) => ShowPromotion(to)
      case _ => NoEvent
    }

    val checkEvents: IndexedSeq[IOEvent] = movedBoard.doOnCheck(pos => ShowCheck(pos), NoEvent)

    if (isValid) Some(Output(movedBoard.clone(gameStatus = updatedStatus), checkEvents ++ Array(endedEvent, promoEvent)))
    else None
  } else None

  /**
    * The actual move operation without any tests.
    * This method trusts its inputs so you should always use [[chess.framework.ChessBoard#move move]]
    * if you are not fully sure the input is correct.
    * @param from start
    * @param to end
    * @param piece the moving piece
    * @param startColor color of the piece
    * @param endColor color of the piece on the end square
    * @return the board after the move
    */
  private def doMove(from: Square, to: Square, piece: Piece, startColor: Color, endColor: Color): ChessBoard = {
    val updatedStatus: GameStatus = piece match {
      case Pawn(color, _) if to.row == ClassicalValues.piecesStartLine(color.opposite) =>
        PromoReq(to)
      case _ =>
        StandardReq
    }

    //adds the currently evaluated move to the history
    val updatedHistory: List[MoveData] = MoveData(from, piece, to, startColor.opposite == endColor) :: history

    var result: ChessBoard = clone(history = updatedHistory, turn = turn.opposite, gameStatus = updatedStatus)

    //testing for en passant and castling
    piece match {
      case Pawn(_, _) if apply(to).isEmpty && from.column != to.column =>
        result = result.emptySquare(Square(to.column, from.row))
      case King(color, moved)
        if !moved && from == ChessBoard.ClassicalValues.kingStartSquare(turn) &&
          (to.column == 'c' || to.column == 'g') =>
        val row = if (color == White) 1 else 8
        val col = if (to.column == 'c') 'd' else 'f'
        val emptyCol = if (to.column == 'c') 'a' else 'h'
        val res = result.updated(Square(col, row), Rook(color)).emptySquare(Square(emptyCol, row))
        result = res
      case _ =>
    }

    val resPiece = Piece(piece.identifier, piece.color, moved = true)
    result.updated(to, resPiece).emptySquare(from)
  }

  /** Tests if a player is currently checked. */
  def isCheck(color: AnyColor = turn): Boolean = checkedSquares(color) nonEmpty

  /**
    * Whether [[chess.framework.ChessBoard#turn turn]]'s king is checked.
    * Use this instead of `isCheck()` or `isCheck(turn)` to improve performance.
    */
  @inline
  lazy val isCheck: Boolean = isCheck()

  /**
    * Calls a function for every checked square (a square with a checked king on it).
    * @param func      a method that will be called when the player is checked
    * @param onFailure a return value if the player is not checked
    * @param color     the player's color
    * @tparam T some return type
    * @return either the result of `func` or `onFailure`
    */
  def doOnCheck[T](func: Square => T, onFailure: T, color: AnyColor = turn): IndexedSeq[T] = {
    val sqrs = checkedSquares()
    if (sqrs nonEmpty) sqrs map func
    else IndexedSeq(onFailure)
  }

  /**
    * Finds all checked kings of one color.
    *
    * @param color kings of this color are tested
    * @return a list of all positions with checked kings
    */
  def checkedSquares(color: AnyColor = turn): IndexedSeq[Square] = {
    for {
      c <- 1 to 8
      row <- 1 to 8
      sqr = Square(columnLetter(c), row)
      piece = apply(sqr)
      if piece === King(color) && isAttacked(sqr, withKing = true)
    } yield sqr
  }

  /**
    * Tests if this is a correct move under consideration of the moved piece's type.
    *
    * @note When capturing en passant the endPiece should be an empty square.
    * @param start      the start square
    * @param end        the ending square
    * @param startPiece the moved piece
    * @param endPiece   the captured piece or [[chess.framework.NoPiece NoPiece]] for an empty square
    * @return `true` if the move is legal, otherwise `false`
    */
  def isLegalMove(start: Square, end: Square, startPiece: Piece, endPiece: Piece): Boolean = {
    val startCIndex = start.colIndx
    val endCIndex = end.colIndx
    val columnDif = endCIndex - startCIndex
    val lineDif = end._2 - start._2

    startPiece match {
      case Pawn(color, moved) =>
        val direction = ClassicalValues.pawnDir(color)
        if (endPiece nonEmpty) (columnDif == 1 || columnDif == -1) && lineDif == direction
        else (columnDif == 0 && (lineDif == direction || (!moved && lineDif == 2 * direction && apply(start + (0, direction)).isEmpty))) ||
          //en passant
          history.nonEmpty && {
            val piece = history.head.piece
            val sPos = history.head.startPos
            val ePos = history.head.endPos
            piece === Pawn(color.opposite) &&
              sPos == Square(end.column, ClassicalValues.pawnStartLine(color.opposite)) &&
              ePos == sPos + (0, -2 * direction) &&
              lineDif == direction &&
              (columnDif == 1 || columnDif == -1) &&
              end == ePos + (0, direction)
          }
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
            val squaresToTest: List[NumericSquare] = sqr2indxSqr(Square(if (startCIndex < endCIndex) 'g' else 'c', start.row)) to start

            def isSqrAttacked(sqr: AbstractSqrCoordinate[_]): Boolean = sqr match {
              case square: Square => isAttacked(square, turn, withKing = true)
              case square: NumericSquare => isAttacked(square, turn, withKing = true)
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
    * @see [[chess.framework.ChessBoard#isAttacked isAttacked]]
    * @return `true` if the square is attacked, otherwise `false`
    */
  def isAttacked(sqr: Square, withKing: Boolean): Boolean = {
    val attackedCol = apply(sqr).color
    attackedCol match {
      case col: AnyColor => isAttacked(sqr, col, withKing)
      case NoColor => false
    }
  }

  /**
    * Tests a square for being attacked.
    *
    * @param sqr      the square that gets tested
    * @param attacked the 'defending' color
    * @return `true` if the square is attacked, otherwise `false`
    */
  def isAttacked(sqr: Square, attacked: AnyColor, withKing: Boolean): Boolean = {
    attackingPieces(sqr, attacked, withKing) > 0
  }

  /**
    * Counts all pieces that are attacking a square.
    *
    * @param sqr      the square that gets tested
    * @param attacked the ''defending'' color
    * @param withKing whether the king should be counted as possible attacker
    * @return `true` if the square is attacked, otherwise `false`
    */
  def attackingPieces(sqr: Square, attacked: AnyColor, withKing: Boolean = true): Int = {
    implicit class Intersectable[P <: Piece](val content: Array[P]) {
      def ^[OtherP <: Piece](other: Array[OtherP]): Int = (for (i <- content; j <- other) yield if (j === i) 1 else 0) sum
    }

    val opponent = attacked.opposite
    val colI = sqr.colIndx
    val row = sqr.row

    def partNxtPiece(inc: (Int, Int)): Piece = nextPiece(NumericSquare(colI, sqr.row), NumericSquare(inc))

    def partApply(inc: (Int, Int)) = apply(NumericSquare(colI, row) + inc)

    val attackedByKnight: Int =
      Array(partApply(1, 2), partApply(2, 1), partApply(2, -1), partApply(1, -2), partApply(-1, 2), partApply(-2, 1), partApply(-2, -1), partApply(-1, -2)) count (Knight(opponent) === _)

    val attackedByKing: Int =
      if (withKing) Array.apply(partApply(1, 1), partApply(-1, 1), partApply(1, -1), partApply(-1, -1), partApply(0, 0), partApply(-1, 0), partApply(1, 0), partApply(0, -1), partApply(0, 1)) count (King(opponent) === _)
      else 0

    val attackedDiagonally: Int =
      Array(Queen(opponent), Bishop(opponent)) ^ Array(partNxtPiece(1, 1), partNxtPiece(-1, -1), partNxtPiece(1, -1), partNxtPiece(-1, 1))

    val attackedOrthogonally: Int =
      Array(Queen(opponent), Rook(opponent)) ^ Array(partNxtPiece(1, 0), partNxtPiece(-1, 0), partNxtPiece(0, -1), partNxtPiece(0, 1))

    val attackedByPawn: Int = {
      val dir = ClassicalValues.pawnDir(opponent.opposite)

      def pieceAtOffset(colD: Int, rowD: Int): Piece = apply(NumericSquare(colI, row) + (colD, rowD))

      val arr = Array(pieceAtOffset(1, dir), pieceAtOffset(-1, dir))

      arr count (_ === Pawn(opponent))
    }

    attackedByKnight + attackedByKing + attackedDiagonally + attackedOrthogonally + attackedByPawn
  }

  def attackingSquares(sqr: Square, attacked: AnyColor, withKing: Boolean = true): Array[(Square, AnyPiece)] = {
    val opponent = attacked.opposite
    allPieces
      .filter (_._2.color == opponent) // filter all pieces of the correct color
      .filter (ap => withKing || (ap._2 !== King(opponent))) // exclude all kings if withKing is true
      .filter (a => isLegalMove(a._1, sqr, a._2, apply(sqr))) // filter for all pieces that can move to that square
  }

  /**
    * Tests if there is a piece on a specific square that is pinned (it blocks an attack against the king)
    * and thereby cannot be moved.
    */
  def isPinnedPiece(square: Square): Boolean = getPiece(square) match {
    case Some(piece) =>
      piece match {
        case NoPiece => false
        case King(_, _) => false
        case anyPiece: AnyPiece =>
          val kings = allPieces filter (_._2 === King(anyPiece.color))
          kings exists { king =>
            val negVec = NumericSquare((square.colIndx - king._1.colIndx).signum, (square.row - king._1.row).signum)

            val possiblePinner = nextPiece(square, negVec)

            if (isEmptyDiagonal(king._1, square)) possiblePinner match {
              case Bishop(_, _) => true
              case Queen(_, _) => true
              case _ => false
            }

            else if (isEmptyOrthogonal(king._1, square)) possiblePinner match {
              case Rook(_, _) => true
              case Queen(_, _) => true
              case _ => false
            }
            else false
          }
      }
    case None => false
  }

  /**
    * Tests if the 50 moves rule applies to this game.
    * This means that in the last 50 moves no pawn was moved and no piece was captured.
    */
  def isFiftyMovesRuleApplying: Boolean =
    movesSinceLastCapture / 2 > 50 && movesSinceLastPawnMove / 2 > 50

  /**
    * @return the number of moves since the last time a capture happened.
    */
  private def movesSinceLastCapture: Int = {
    val index = history.indexWhere(_.captured)
    if (index == -1) history.length
    else index + 1
  }

  /**
    * @return the amount of moves since the last time a pawn was moved
    */
  private def movesSinceLastPawnMove: Int = {
    val index = history.indexWhere(_.piece.isInstanceOf[Pawn])
    if (index == -1) history.length
    else index + 1
  }

  /**
    * Tests if a certain [[chess.framework.Square]] is blocked,
    * i.e. the piece on the square cannot move.
    * When the square is empty, `false` is returned.
    */
  def isBlockedSquare(square: Square): Boolean = {
    val piece = apply(square)

    def atOffset(off: (Int, Int)): Square = square + off

    def offsetPiece(off: (Int, Int)) = getPiece(atOffset(off))

    if (isPinnedPiece(square)) true
    else piece match {
      case Pawn(color, _) =>
        val dir = ClassicalValues.pawnDir(color)
        val opponent: AnyColor = color.opposite
        val capturing: Boolean = (offsetPiece(1, dir) match {
          case Some(offPiece) => offPiece.color == opponent
          case None => false
        }) || (offsetPiece(-1, dir) match {
          case Some(offPiece) => offPiece.color == opponent
          case None => false
        })
        val moving = offsetPiece(0, dir).getOrElse(NoPiece).isEmpty
        val enPassant = history.nonEmpty && {
          val piece = history.head.piece
          val sPos = history.head.startPos
          val ePos = history.head.endPos
          piece === Pawn(opponent) && (ePos == atOffset(1, 0) || ePos == atOffset(-1, 0)) && ePos == sPos + (0, -2 * dir)
        }
        !capturing && !moving && !enPassant
      case King(color, _) =>
        val moves = Array(
          0 -> 1, 0 -> -1,
          1 -> 0, 1 -> -1, 1 -> 1,
          -1 -> 0, -1 -> 1, -1 -> -1
        )
        !moves.exists(d => !isAttacked(atOffset(d), color, withKing = false)
          && offsetPiece(d)
          .flatMap(offPiece => Option(offPiece.color != color))
          .getOrElse(false))
      case other if other.isInstanceOf[AnyPiece] =>
        val offsets = other match {
          case Knight(_, _) =>
            Array(
              1 -> 2, 1 -> -2,
              -1 -> 2, -1 -> -2,
              2 -> 1, 2 -> -1,
              -2 -> 1, -2 -> -1
            )
          case Queen(_, _) =>
            Array(
              0 -> 1, 0 -> -1,
              1 -> 0, 1 -> -1, 1 -> 1,
              -1 -> 0, -1 -> 1, -1 -> -1
            )
          case Bishop(_, _) =>
            Array(
              1 -> 1, 1 -> -1,
              -1 -> 1, -1 -> -1
            )
          case Rook(_, _) =>
            Array(
              0 -> 1, 0 -> -1,
              1 -> 0, -1 -> 0
            )
          case _ =>
            Array.empty
        }
        !offsets.exists(i => offsetPiece(i)
          .flatMap(piece => Option(piece.color != other.color))
          .getOrElse(false)
        )
      case NoPiece => false
    }
  }

  /**
    * Tests for blockage by testing if every piece is blocked
    * and the kings are unable to reach any opponent piece.
    */
  def isBlocked: Boolean = {
    def piecesBlocked =
      allPieces
        .filterNot(_._2.isInstanceOf[King])
        .map(_._1)
        .forall(isBlockedSquare)

    def kingsBlocked = {
      val kings: Array[(Square, AnyPiece)] = allPieces filter (_._2.isInstanceOf[King]) reverse

      def kingIsBlocked(kingOnSq: (Square, AnyPiece)): Boolean = {
        import WaypointResult._
        import pathfinding.KingMovementPathfinder
        val pos = kingOnSq._1
        val king = kingOnSq._2

        val kingColor = king.color

        val pathfinderRes = new KingMovementPathfinder {
          override def decision(pos: Square): WaypointResult.Value = getPiece(pos) match {
            case Some(piece) if !isAttacked(pos, kingColor, withKing = false) && kingColor != piece.color =>
              piece.color match {
                case color: AnyColor if color == kingColor.opposite => Positive
                case _ => Continuation
              }
            case _ => Termination
          }
        }.apply(pos)

        !pathfinderRes.isSuccess
      }

      kings forall kingIsBlocked
    }

    piecesBlocked && kingsBlocked
  }

  /** Tests for mate. */
  def isMate: Boolean =
    allPieces
      .filter (_._2 === King(turn)) // filters for kings of the right color
      .exists { testedKing =>
        val kingColor = turn
        val kingSq = testedKing._1
        val alliedPieces = allPieces.filter(piece => {
          piece._2.color == kingColor && (piece._2 !== King(kingColor))
        })

        //tests if any piece can move to a specific position (-> block or capture)
        def piecesCanMoveTo(sqr: Square): Boolean = {
          val endPiece = apply(sqr)
          alliedPieces exists (sp => !isPinnedPiece(sp._1) && isLegalMove(sp._1, sqr, sp._2, endPiece))
        }

        def adjacents = kingSq.validAdjacents

        def kingCanMove = adjacents exists (a => {
          val adjacentPiece = apply(a)
          !isAttacked(a, kingColor, withKing = true) && adjacentPiece.color != kingColor && !doMove(kingSq, a, testedKing._2, kingColor, adjacentPiece.color).isCheck(kingColor)
        })

        def canBlock: Boolean =
          attackingPieces(kingSq, kingColor, withKing = false) <= 1 && {
            // it can be safely assumed here that the amount of attacking pieces is exactly 1
            val attacker = attackingSquares(kingSq, kingColor, withKing = false).head
            attacker._2 match {
              case Knight(_, _) => piecesCanMoveTo(attacker._1)
              case _ => (attacker._1 until kingSq) exists piecesCanMoveTo
            }
          }

        isAttacked(kingSq, withKing = false) && !kingCanMove && !canBlock
      }

  /**
    * Tests for stalemate (when a player is not checked but cannot move
    * because every possible move would result in him being checked).
    */
  def isStalemate: Boolean = !isCheck && {
    allPieces
      .filter(p => p._2.color == turn)
      .map(_._1)
      .forall (isBlockedSquare)
  }

  /**
    * Tests for insufficient material of any color.
    *
    * @see [[chess.framework.ChessBoard#isInsufficientMaterial(color: chess\.framework\.AnyColor)* isInsufficientMaterial]]
    */
  def isInsufficientMaterial: Boolean = isInsufficientMaterial(Black) && isInsufficientMaterial(White)

  /**
    * Tests if a specific color has not enough material to mate its opponent.
    * This occurs when one of the following cases is true:
    *
    *   1. no pieces
    *   2. only bishops of the same color
    *   3. only one knight
    *
    * @return `true` if there is not enough material, otherwise `false`
    */
  def isInsufficientMaterial(color: AnyColor): Boolean = {
    val piecesOfColor = // pieces of specified color without kings
      allPieces
        .map { tup => /*replace square with square color*/ (if (tup._1.colIndx % 2 == tup._1.row % 2) Black else White, tup._2) }
        .filter (_._2.color == color)
        .filterNot (_._2 === King(color))

    val pieces = piecesOfColor map (_._2) // actual pieces

    val value: Int = (pieces map (_.value)).sum // sum of all values

    def existsPawn = pieces.exists(_ === Pawn(color))

    def existsMajorPiece = pieces.exists(p => p === Rook(color) || p === Queen(color))

    (value == 0) || {
      !existsMajorPiece && !existsPawn && {
        val bishops = piecesOfColor filter (_._2 === Bishop(color))
        val knights = pieces filter (_ === Knight(color))

        def bishopsOfSameColor = bishops.exists(_._1 == White) != bishops.exists(_._1 == Black)

        (knights.isEmpty && bishopsOfSameColor) || (bishops.isEmpty && knights.length == 1)
      }
    }
  }

  /** Tests for a 5x repetition of the same position. */
  def isFivefoldRepetition: Boolean =
    positions.maxRepetition >= 5

  /**
    * Searches for the next piece.
    *
    * @usecase useful for searching on an diagonal or orthogonal
    * @note Returns [[chess.framework.NoPiece NoPiece]] when no piece is found.
    * @param start     the start square
    * @param increment the incrementation every iteration
    * @return the first piece on a line
    */
  @tailrec
  final def nextPiece(start: NumericSquare, increment: NumericSquare): Piece = {
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
    * @see [[chess.framework.ChessBoard#isEmptyDiagonal isEmptyDiagonal]]
    */
  def isEmptyOrthogonal(from: Square, to: Square): Boolean = {
    val orthogonal = from._1 == to._1 || from._2 == to._2
    orthogonal && isEmptyConnection(from, to)
  }

  /**
    * Tests if two squares are located on the same diagonal and the next piece on this diagonal is the piece on the end-square.
    *
    * @see [[chess.framework.ChessBoard#isEmptyOrthogonal isEmptyOrthogonal]]
    */
  def isEmptyDiagonal(from: Square, to: Square): Boolean = {
    val startColIndex = from.colIndx
    val endColIndex = to.colIndx
    val diagonal = startColIndex - endColIndex == from._2 - to._2 || startColIndex - to._2 == endColIndex - from._2
    diagonal && isEmptyConnection(from, to)
  }

  /**
    * Updates the board.
    *
    * @param square the coordinate of the square to updated
    * @param piece  the piece the square shall be updated to
    * @return a ChessBoard with updated squares.
    */
  def updated(square: Square, piece: Piece): ChessBoard = {
    val updated = squares(square._1).updated(square._2, piece)
    clone(squares = squares.updated(square._1, updated))
  }

  @tailrec
  private def isEmptyConnection(from: Square, to: Square): Boolean = {
    val startColIndex = from.colIndx
    val endColIndex = to.colIndx
    val incremented: Square = NumericSquare((endColIndex - startColIndex).signum, (to._2 - from._2).signum) + from
    if (incremented == to) true
    else if (apply(incremented).isEmpty) isEmptyConnection(incremented, to)
    else false
  }
}


object ChessBoard {
  /**
    * The data-version; used to verify `.save` files (stored games/ boards).
    *
    * @note this constant will be updated with every update changing the way of saving [[chess.framework.ChessBoard ChessBoard]]s.
    * @version alpha 0.1
    */
  val Version = 1L

  /**
    * @return An empty chess board
    */
  def empty(implicit io: ChessIO): ChessBoard = fill(NoPiece)

  /**
    * Fills a 8 * 8 board with a specific piece.
    *
    * @return a fully filled [[chess.framework.ChessBoard ChessBoard]]
    */
  def fill(piece: Piece)(implicit io: ChessIO): ChessBoard = this (Array.fill(8)(Column(piece)), Nil, Positions.empty, White).get

  /**
    * Defines the classical chess standard board
    *
    * @return a chess board with the standard start position
    */
  def classicalBoard(implicit io: ChessIO): ChessBoard = ChessBoard(
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
    ), Nil, Positions.empty, White, StandardReq
  )

  /**
    * Generator for chessboards.
    */
  def apply(columns: Array[Column], history: List[MoveData] = Nil, positions: Positions = Positions.empty, turn: AnyColor = White)(implicit io: ChessIO): Option[ChessBoard] =
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
        ), history, positions, turn, StandardReq
      ))
    else None

  /**
    * Saves a [[chess.framework.ChessBoard]] to a file using the xml format.
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
    * Loads a [[chess.framework.ChessBoard]] from a file and adds `.save`
    * if the path does not contain any file extension.
    *
    * @see [[chess.framework.ChessBoard#loadExactPath loadExactPath]]
    * @param path the file
    * @return a board or [[scala.None]] when the board was saved in a different version.
    */
  def load(path: String)(implicit io: ChessIO): Either[LoadingError.LoadingError, ChessBoard] = {
    val fullPath = path + (if (path contains ".") "" else ".save")
    loadExactPath(fullPath)
  }

  /**
    * Loads a board from a path without alternating it.
    * @see [[chess.framework.ChessBoard#load load]]
    */
  def loadExactPath(path: String)(implicit io: ChessIO): Either[LoadingError.LoadingError, ChessBoard] =
    try SaveLoader.load(xml.XML.load(path))
    catch {
      case _: Throwable =>
        Left(LoadingError.FileNotFoundError(path))
    }

  def saveSquares(squares: Map[Char, Column]): NodeSeq =
    for (x <- 1 to 8; col = columnLetter(x)) yield <col>{squares(col).saveData}</col> copy (label = col.toUpper toString)

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

  /** @return `true` if a column with this identifier does exist, else `false` */
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
    * Standard values for a classical chess game.
    */
  object ClassicalValues {
    /** @return the row where all pawns of this color are placed. */
    def pawnStartLine(color: AnyColor): Int =
      piecesStartLine(color) + pawnDir(color)

    /** @return the [[chess.framework.Square]] where the king of this color is placed. */
    def kingStartSquare(color: AnyColor): Square =
      Square('e', piecesStartLine(color))

    /** @return the row where all pieces of this color are generated. */
    def piecesStartLine(color: AnyColor): Int =
      if (color == White) 1 else 8

    /**
      * The moving-direction of pawns of this color (i.e. the line difference they are taking in every normal move)
      *
      * @return `1` for [[chess.framework.White]] and `-1` for [[chess.framework.Black]]
      */
    def pawnDir(color: AnyColor): Int =
      if (color == White) 1 else -1
  }

}
