package framework

import framework.BoardStatus.GameResult._
import framework.BoardStatus.GameStatus.{GameStatus, PromoReq, StandardReq, _}
import framework.BoardStatus.ResultReason.DrawResultReason._
import framework.BoardStatus.ResultReason.WinResultReason._
import framework.IOEvents._
import framework.Input._
import framework.pathfinding.WaypointResult

import scala.language.postfixOps
import scala.xml._

/**
  * Describes a chess game (I know it is called ChessBoard and
  * not ChessGame but would you please just shut the fuck up
  * and leave me alone? EVER HEARD OF CREATIVE FREEDOM?!?!) by
  * combining relevant information about the state of the game
  * with logic to manipulate those and a [[framework.ChessIO]]
  * used as a channel for input to and output from the game.
  *
  * Defines a classical 1 vs 1 chess board
  * and defines an access via the `receive` method.
  * Use the companion object to initialize.
  *
  * @constructor Should be self-descriptive so if you do not get it
  *              it is your fault.
  * @version alpha 0.3
  * @author Felix Lehner
  */
case class ChessBoard(
                       squares: BoardMap,
                       history: List[MoveData],
                       positions: Positions,
                       turn: AnyColor,
                       gameStatus: GameStatus
                     )(implicit val io: ChessIO, val startPos: StartPosition = ArbitraryPosition(squares)) {

  import ChessBoard._

  /** All pieces (excluding [[framework.NoPiece NoPieces]]) and their position */
  lazy val allPieces: Array[(Sqr, AnyPiece)] = {
    val allSquares = for {
      x <- 1 to 8
      col = columnLetter(x)
      row <- 1 to 8
      square = Sqr(col, row)
      piece: Piece = apply(square)
      if piece.nonEmpty
    } yield (square, piece)
    allSquares.toArray.asInstanceOf[Array[(Sqr, AnyPiece)]]
  }

  /**
    * Whether [[framework.ChessBoard#turn turn]]'s king is checked.
    * Use this instead of `isCheck()` or `isCheck(turn)` to improve performance.
    */
  lazy val isCheck: Boolean = isCheck()

  /** The move counter */
  val turnCounter: Int = history.length / 2

  /**
    * Handles different input types depending on the [[framework.ChessBoard#gameStatus gameStatus]].
    *
    * @param input some [[framework.Input Input]]
    * @return some [[framework.Output]] or [[scala.None]] when the input is either unknown
    *         or does not match the current input requirements given by [[framework.ChessBoard#gameStatus gameStatus]].
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
        val res = Draw by DrawAgreement
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

  /**
    * Applies a function to all pieces and returns the results.
    */
  def mapPiece[T](func: (Sqr, AnyPiece) => T): IndexedSeq[T] =
    for {
      col <- 1 to 8
      column = columnLetter(col)
      row <- 1 to 8
      square = Sqr(column, row)
      piece = apply(square)
      if piece.nonEmpty
    } yield func(square, piece.asInstanceOf[AnyPiece])

  /**
    * Gives the piece at any position.
    * When the coordinate is outside the board, [[framework.NoPiece NoPiece]] is returned.
    *
    * @param sqr coordinates on the board
    * @return the piece at some specified position
    */
  @inline
  def apply(sqr: Sqr): Piece = getPiece(sqr) getOrElse NoPiece

  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[scala.None]] if the sqr does not exist
    */
  @inline
  def getPiece(sqr: Sqr): Option[Piece] = squares.getPiece(sqr)

  /**
    * Filters for all pieces that match a predicate.
    */
  def filterPieces(func: Piece => Boolean): BoardMap = squares.filter(func)

  /**
    * Generates a new [[framework.ChessBoard ChessBoard]] which shares all attributes with this one
    * but all that are defined in the parameters.
    *
    * @usecase Used to change specific values of the board.
    * @return the new board
    */
  @inline
  def clone(
             squares: BoardMap = this.squares,
             history: List[MoveData] = this.history,
             positions: Positions = this.positions,
             turn: AnyColor = this.turn,
             io: ChessIO = this.io,
             startPos: StartPosition = this.startPos,
             gameStatus: GameStatus = this.gameStatus
           ): ChessBoard = ChessBoard(squares, history, positions, turn, gameStatus)(io, startPos)

  /**
    * Stores this object in the xml format.
    *
    * @note The version attribute is used when loading to find the right loading method.
    * @usecase Used to save&load [[framework.ChessBoard ChessBoard]]s
    * @see [[framework.SaveLoader]]
    * @see [[framework.ChessBoard#save]]
    * @return an xml-[[scala.xml.Elem element]] with all relevant data of the [[framework.ChessBoard]]
    */
  def save: Elem =
    <chessboard version={Version.toString}>
      <startPosition>
        {startPos.xml}
      </startPosition>
      <moves>
        {history map (_.xml)}
      </moves>
      <turn>
        {turn}
      </turn>
      <boardStatus>
        {gameStatus}
      </boardStatus>
    </chessboard>

  /**
    * Formats the board as a [[String]].
    *
    * @usecase Used in consoleUI to show the board in console
    * @see [[framework.BoardMap#toString]]
    * @return a formatted representation of the board
    */
  @inline
  override def toString: String = squares.toString

  /**
    * Empties a square.
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: Sqr): ChessBoard = clone(squares = squares.emptySquare(sqr))

  /**
    * Moves a piece after testing for validity of the move which depends on the following aspects:
    *
    * -both squares have to be on the board
    * -you must capture an enemy piece or move on an empty square
    * -the moved piece must be of the color [[framework.ChessBoard#turn turn]]
    * -it must be a legal movement of the type of piece
    * -the player must not be checked after the move.
    *
    * When the move is legal, the pieces and the turn are changed and
    * the move is added to the [[framework.ChessBoard#history history]].
    *
    * @param from the start square
    * @param to   the end-coordinates
    * @return the updated board
    */
  def move(from: Sqr, to: Sqr): Option[Output] = if (squares.isValid(from) && squares.isValid(to)) {
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

    if (isValid) {
      val updatedStatus: GameStatus =
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

      val endedEvent: IOEvent = updatedStatus match {
        case res: Ended => ShowEnded(res.result)
        case _ => NoEvent
      }

      val promoEvent: IOEvent = movingPiece match {
        case Pawn(color, _) if to.row == ClassicalValues.piecesStartLine(color.opposite) => ShowPromotion(to)
        case _ => NoEvent
      }

      val checkEvents: IndexedSeq[IOEvent] = movedBoard.doOnCheck(pos => ShowCheck(pos), NoEvent)

      val resBoard = movedBoard.clone(gameStatus = updatedStatus)
      val events = checkEvents ++ Array(endedEvent, promoEvent)
      Output(resBoard, events) asSome
    } else None
  } else None

  /**
    * Takes the last move back.
    * If any of the players was checked, also a [[framework.IOEvents.ShowCheck ShowCheck]] event is triggered.
    */
  private[framework] def takeback: Option[Output] =
    if (positions.length >= 1) {
      val resBoard = clone(
        squares = positions.head.pos,
        positions = positions --,
        history = history.tail,
        gameStatus = StandardReq,
        turn = turn.opposite)
      val takebackCheckEvents = resBoard.doOnCheck(pos => ShowCheck(pos), NoEvent)
      Output(
        resBoard,
        Array(RemoveTakeback) ++ takebackCheckEvents
      ) asSome
    }
    else None

  /**
    * Resigns the game, i.e. grants the win to the opposite color.
    */
  private[framework] def resign: Ended =
    Ended(Win(turn.opposite)(Resignation))

  /**
    * Promotes a pawn to a given piece.
    *
    * @param piece the piece's apply method
    * @return an updated [[framework.ChessBoard ChessBoard]] of [[scala.None]] when the piece type is incorrect
    */
  private[framework] def promote(piece: (AnyColor, Boolean) => AnyPiece): Option[Output] = {
    val promoColor = turn.opposite
    val promoPiece = piece match {
      case Queen =>
        Queen(promoColor)
      case Bishop =>
        Bishop(promoColor)
      case Knight =>
        Knight(promoColor)
      case Rook =>
        Rook(promoColor)
      case _ => NoPiece
    }

    if (promoPiece != NoPiece) {
      gameStatus match {
        case PromoReq(sqr: Sqr) =>
          val updatedHistory =
            PromotionMove(history.head.startPos, history.head.piece, sqr, history.head.captured, promoPiece) :: this.history.tail
          val result = updated(sqr, promoPiece).clone(gameStatus = StandardReq, history = updatedHistory)
          val promoCheckEvents: IndexedSeq[IOEvent] =
            result.doOnCheck[Array[IOEvent]](pos => Array(ShowCheck(pos)), Array()).flatten

          // checkmate
          if (promoCheckEvents.nonEmpty && result.isMate) {
            val status = Ended(turn match {
              case White => WhiteWins by Mate
              case Black => BlackWins by Mate
            })

            Output(result.clone(gameStatus = status), Array(RemovePromotion, ShowEnded(status.result))) asSome
          } else Output(result, RemovePromotion +: promoCheckEvents) asSome
        case _ => None
      }
    }
    else None
  }

  /** Tests if a player is currently checked. */
  def isCheck(color: AnyColor = turn): Boolean = checkedSquares(color) nonEmpty

  /**
    * Calls a function for every checked square (a square with a checked king on it).
    *
    * @param func      a method that will be called when the player is checked
    * @param onFailure a return value if the player is not checked
    * @param color     the player's color
    * @tparam T some return type
    * @return either the result of `func` or `onFailure`
    */
  def doOnCheck[T](func: Sqr => T, onFailure: T, color: AnyColor = turn): IndexedSeq[T] = {
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
  def checkedSquares(color: AnyColor = turn): IndexedSeq[Sqr] = {
    for {
      c <- 1 to 8
      row <- 1 to 8
      sqr = Sqr(columnLetter(c), row)
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
    * @param endPiece   the captured piece or [[framework.NoPiece NoPiece]] for an empty square
    * @return `true` if the move is legal, otherwise `false`
    */
  def isLegalMove(start: Sqr, end: Sqr, startPiece: Piece, endPiece: Piece): Boolean = {
    val startCIndex = start.column
    val endCIndex = end.column
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
              sPos == Sqr(end.column, ClassicalValues.pawnStartLine(color.opposite)) &&
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
      case King(color, moved) =>
        (columnDif <= 1 && columnDif >= -1 && lineDif <= 1 && lineDif >= -1) ||
          //castle
        (!moved && (end.column == columnIndex('c') || end.column == columnIndex('g')) && {
            val rookCol = if (end.column == columnIndex('c')) columnIndex('a') else columnIndex('h')
            val rook = apply(Sqr(rookCol, ClassicalValues.piecesStartLine(color)))
            val squaresToTest: List[Sqr] =
              Sqr(if (startCIndex < endCIndex) columnIndex('g') else columnIndex('c'), start.row) to start

            def isSqrAttacked(sqr: Sqr): Boolean = isAttacked(sqr, turn, withKing = true)

            rook == Rook(color, false) && squaresToTest.forall(sqr => !isSqrAttacked(sqr)) && isEmptyOrthogonal(start, end)
          })
      case NoPiece => false
    }
  }

  /**
    * An overloading method. It takes the color of the piece on the square as
    * the ''defender'', i.e. the not-attacking color.
    *
    * @see [[framework.ChessBoard#isAttacked isAttacked]]
    * @return `true` if the square is attacked, otherwise `false`
    */
  def isAttacked(sqr: Sqr, withKing: Boolean): Boolean = {
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
  def isAttacked(sqr: Sqr, attacked: AnyColor, withKing: Boolean): Boolean =
    attackingPieces(sqr, attacked, withKing) > 0

  /**
    * Counts all pieces that are attacking a square.
    *
    * @param sqr      the square that gets tested
    * @param attacked the ''defending'' color
    * @param withKing whether the king should be counted as possible attacker
    * @return `true` if the square is attacked, otherwise `false`
    */
  def attackingPieces(sqr: Sqr, attacked: AnyColor, withKing: Boolean = true): Int = {
    implicit class Intersectable[P <: Piece](val content: Array[P]) {
      def ^[OtherP <: Piece](other: Array[OtherP]): Int = (for (i <- content; j <- other) yield if (j === i) 1 else 0) sum
    }

    val opponent = attacked.opposite

    def partNxtPiece(inc: (Int, Int)): Piece = nextPiece(sqr, Sqr(inc))

    def partApply(inc: (Int, Int)) = apply(sqr + inc)

    val attackedByKnight: Int =
      Array(partApply(1, 2), partApply(2, 1), partApply(2, -1), partApply(1, -2), partApply(-1, 2),
        partApply(-2, 1), partApply(-2, -1), partApply(-1, -2)) count (Knight(opponent) === _)

    val attackedByKing: Int =
      if (withKing) Array.apply(partApply(1, 1), partApply(-1, 1), partApply(1, -1), partApply(-1, -1), partApply(0, 0),
        partApply(-1, 0), partApply(1, 0), partApply(0, -1), partApply(0, 1)) count (King(opponent) === _)
      else 0

    val attackedDiagonally: Int =
      Array(Queen(opponent), Bishop(opponent)) ^ Array(partNxtPiece(1, 1), partNxtPiece(-1, -1),
        partNxtPiece(1, -1), partNxtPiece(-1, 1))

    val attackedOrthogonally: Int =
      Array(Queen(opponent), Rook(opponent)) ^ Array(partNxtPiece(1, 0), partNxtPiece(-1, 0),
        partNxtPiece(0, -1), partNxtPiece(0, 1))

    val attackedByPawn: Int = {
      val dir = ClassicalValues.pawnDir(attacked)

      def pieceAtOffset(colD: Int, rowD: Int): Piece = apply(sqr + (colD, rowD))

      val arr = Array(pieceAtOffset(1, dir), pieceAtOffset(-1, dir))

      arr count (_ === Pawn(opponent))
    }

    attackedByKnight + attackedByKing + attackedDiagonally + attackedOrthogonally + attackedByPawn
  }

  def attackingSquares(sqr: Sqr, attacked: AnyColor, withKing: Boolean = true): Array[(Sqr, AnyPiece)] = {
    val opponent = attacked.opposite
    allPieces
      .filter(_._2.color == opponent) // filter all pieces of the correct color
      .filter(ap => withKing || (ap._2 !== King(opponent))) // exclude all kings if withKing is true
      .filter(a => isLegalMove(a._1, sqr, a._2, apply(sqr))) // filter for all pieces that can move to that square
  }

  /**
    * Tests if there is a piece on a specific square that is pinned (it blocks an attack against the king)
    * and thereby cannot be moved.
    */
  def isPinnedPiece(square: Sqr): Boolean = getPiece(square) match {
    case Some(piece) =>
      piece match {
        case NoPiece => false
        case King(_, _) => false
        case anyPiece: AnyPiece =>
          val kings = allPieces filter (_._2 === King(anyPiece.color))
          kings exists { king =>
            val negVec = Sqr((square.column - king._1.column).signum, (square.row - king._1.row).signum)

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
    * Tests if a certain [[framework.Sqr]] is blocked,
    * i.e. the piece on the square cannot move.
    * When the square is empty, `false` is returned.
    */
  def isBlockedSquare(square: Sqr): Boolean = {
    val piece = apply(square)

    def atOffset(off: (Int, Int)): Sqr = square + off

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
        val adjacents = square.adjacents.filter(squares.isValid)
        !adjacents.exists(sq => !isAttacked(sq, color, withKing = true) && apply(sq).color != color)
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
	 *
	 * If this does not work it's probably... surely caused 
	 * by the abomination I've created in [[framework.pathfinding]].
	 * It actually works in most cases. If you get a false negative
	 * result you just can agree on a draw. In the case of a false
	 * positive just comment all this crap out, return false and
	 * recompile.
	 *
	 * @see [[framework.pathfinding.KingMovementPathfinder]] if
	 *			you actually want to take a look at this mesmerizing
	 *			piece of shit that should have been aborted the very
	 *			moment I thought of doing it this way. Also you can
	 *			find there a more detailed description of the situation.
	 */
  def isBlocked: Boolean = {
    def piecesBlocked =
      allPieces
        .filterNot(_._2.isInstanceOf[King])
        .map(_._1)
        .forall(isBlockedSquare)

    def kingsBlocked = {
      val kings: Array[(Sqr, AnyPiece)] = allPieces filter (_._2.isInstanceOf[King]) reverse

      def kingIsBlocked(kingOnSq: (Sqr, AnyPiece)): Boolean = {
        import framework.pathfinding.KingMovementPathfinder
        import framework.pathfinding.WaypointResult._

        val pos = kingOnSq._1
        val king = kingOnSq._2

        val kingColor = king.color

        val pathfinder = new KingMovementPathfinder {
          override def decision(pos: Sqr): WaypointResult.Value = getPiece(pos) match {
            case Some(piece) if !isAttacked(pos, kingColor, withKing = false) && kingColor != piece.color =>
              piece.color match {
                case color: AnyColor if color == kingColor.opposite => Positive
                case _ => Continuation
              }
            case _ => Termination
          }
        }

        val pathfinderRes = pathfinder.apply(pos)

        !pathfinderRes.isSuccess
      }

      kings forall kingIsBlocked
    }

    piecesBlocked && kingsBlocked
  }

  /** Tests for mate. */
  def isMate: Boolean =
    allPieces
      .filter(_._2 === King(turn)) // filters for kings of the right color
      .exists(kingIsMate)

  /**
    * Tests for stalemate (when a player is not checked but cannot move
    * because every possible move would result in him being checked).
    */
  def isStalemate: Boolean =
    !isCheck &&
      allPieces
        .filter(_._2.color == turn)
        .map(_._1)
        .forall(isBlockedSquare)

  /**
    * Tests for insufficient material of any color.
    *
    * @see [[framework.ChessBoard#isInsufficientMaterial(color: chess\.framework\.AnyColor)* isInsufficientMaterial]]
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
        .map { tup => /*replace square with square color*/
          (if (tup._1.column % 2 == tup._1.row % 2) Black else White, tup._2) }
        .filter(_._2.color == color)
        .filterNot(_._2 === King(color))

    val pieces = piecesOfColor map (_._2) // actual pieces

    val value: Int = (pieces map (_.value)).sum // sum of all values

    def existsPawn = pieces.exists(_ === Pawn(color))

    def existsMajorPiece = pieces.exists(p => p === Rook(color) || p === Queen(color))

    value == 0 || (!existsMajorPiece && !existsPawn && {
      val bishops = piecesOfColor filter (_._2 === Bishop(color))
      val knights = pieces filter (_ === Knight(color))

      def bishopsOfSameColor = bishops.exists(_._1 == White) != bishops.exists(_._1 == Black)

      def existsEnemyPiece(piece: (AnyColor, Boolean) => AnyPiece) =
        allPieces.exists(_._2 === piece(color.opposite, false))

      if (knights.isEmpty) bishopsOfSameColor || (bishops.length == 1 && !existsEnemyPiece(Knight))
      else bishops.isEmpty && knights.length == 1 && !existsEnemyPiece(Bishop)
    })
  }

  /** Tests for a 5x repetition of the same position. */
  def isFivefoldRepetition: Boolean =
    positions.maxRepetition >= 5

  /**
    * Searches for the next piece.
    *
    * @usecase useful for searching on an diagonal or orthogonal
    * @note Returns [[framework.NoPiece NoPiece]] when no piece is found.
    * @param start     the start square
    * @param increment the incrementation every iteration
    * @return the first piece on a line
    */
  @scala.annotation.tailrec
  final def nextPiece(start: Sqr, increment: Sqr): Piece = {
    val incremented = start + increment
    if (squares.isValid(incremented)) apply(incremented) match {
      case NoPiece => nextPiece(incremented, increment)
      case piece => piece
    }
    else NoPiece
  }

  /**
    * Tests if two squares are on the same orthogonal and if the space between them is empty.
    *
    * @see [[framework.ChessBoard#isEmptyDiagonal isEmptyDiagonal]]
    */
  def isEmptyOrthogonal(from: Sqr, to: Sqr): Boolean = {
    val orthogonal = from._1 == to._1 || from._2 == to._2
    orthogonal && isEmptyConnection(from, to)
  }

  /**
    * Tests if two squares are located on the same diagonal
    * and the next piece on this diagonal is the piece on the end-square.
    *
    * @see [[framework.ChessBoard#isEmptyOrthogonal isEmptyOrthogonal]]
    */
  def isEmptyDiagonal(from: Sqr, to: Sqr): Boolean = {
    val startColIndex = from.column
    val endColIndex = to.column
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
  def updated(square: Sqr, piece: Piece): ChessBoard =
    clone(squares = squares.updated(square, piece))

  /**
    * The actual move operation without any tests.
    * This method trusts its inputs so you should always use [[framework.ChessBoard#move move]]
    * if you are not fully sure the input is correct.
    *
    * @param from       start
    * @param to         end
    * @param piece      the moving piece
    * @param startColor color of the piece
    * @param endColor   color of the piece on the end square
    * @return the board after the move
    */
  private def doMove(from: Sqr, to: Sqr, piece: Piece, startColor: Color, endColor: Color): ChessBoard = {
    val updatedStatus: GameStatus = piece match {
      case Pawn(color, _) if to.row == ClassicalValues.piecesStartLine(color.opposite) =>
        PromoReq(to)
      case _ =>
        StandardReq
    }

    //adds the currently evaluated move to the history
    val updatedHistory: List[MoveData] = MoveData(from, piece, to, startColor.opposite == endColor) :: history

    clone(squares = squares.movePiece(from, to, piece),
      history = updatedHistory,
      turn = turn.opposite,
      gameStatus = updatedStatus)
  }

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

  private def kingIsMate(testedKing: (Sqr, AnyPiece)): Boolean = {
    lazy val kingColor = turn
    val kingSq = testedKing._1
    lazy val alliedPieces = allPieces.filter(piece => {
      piece._2.color == kingColor && (piece._2 !== King(kingColor))
    })

    //tests if any piece can move to a specific position (-> block or capture)
    def piecesCanMoveTo(sqr: Sqr): Boolean = {
      val endPiece = apply(sqr)
      alliedPieces exists (sp => !isPinnedPiece(sp._1) && isLegalMove(sp._1, sqr, sp._2, endPiece))
    }

    def adjacents = kingSq.adjacents.filter(squares.isValid)

    def kingCanMove = adjacents exists (a => {
      val adjacentPiece = apply(a)
      !isAttacked(a, kingColor, withKing = true) &&
        adjacentPiece.color != kingColor &&
        !doMove(kingSq, a, testedKing._2, kingColor, adjacentPiece.color).isCheck(kingColor)
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

  @scala.annotation.tailrec
  private def isEmptyConnection(from: Sqr, to: Sqr): Boolean = {
    val startColIndex = from.column
    val endColIndex = to.column
    val incremented: Sqr = Sqr((endColIndex - startColIndex).signum, (to._2 - from._2).signum) + from
    if (incremented == to) true
    else if (apply(incremented).isEmpty) isEmptyConnection(incremented, to)
    else false
  }
}

object ChessBoard {
  /**
    * The data-version; used to verify `.save` files (stored games/ boards).
    *
    * @note this constant will be updated with every update changing the way of saving [[framework.ChessBoard ChessBoard]]s.
    * @version alpha 0.1
    */
  val Version = 2L

  /**
    * @return An empty chess board
    */
  @inline
  def empty(implicit io: ChessIO): ChessBoard = fill(NoPiece)

  /**
    * Fills a 8 * 8 board with a specific piece.
    *
    * @return a fully filled [[framework.ChessBoard ChessBoard]]
    */
  def fill(piece: Piece)(implicit io: ChessIO): ChessBoard =
    this(BoardMap.fill(piece), Nil, Positions.empty, White, StandardReq)

  val classicalPosition: BoardMap = BoardMap(Array(
    Array(Rook(White), Knight(White), Bishop(White), Queen(White), King(White), Bishop(White), Knight(White), Rook(White)),
    Array.fill(8)(Pawn(White)),
    Array.fill(8)(NoPiece),
    Array.fill(8)(NoPiece),
    Array.fill(8)(NoPiece),
    Array.fill(8)(NoPiece),
    Array.fill(8)(Pawn(Black)),
    Array(Rook(Black), Knight(Black), Bishop(Black), Queen(Black), King(Black), Bishop(Black), Knight(Black), Rook(Black))))

  /**
    * Defines the classical chess standard board
    *
    * @return a chess board with the standard start position
    */
  def classicalBoard(implicit io: ChessIO): ChessBoard =
    ChessBoard(classicalPosition, Nil, Positions.empty, White, StandardReq)(io, ClassicPosition)

  /**
    * Saves a [[framework.ChessBoard]] to a file using the xml format.
    *
    * @param fileName a name for the save; technically it's possible to use a file path
    *                 but this does often lead to errors (due to a lazy developer...)
    */
  def save(board: ChessBoard, fileName: String = "save"): Option[FileOperationError.FileNotFoundError] = {
    val boardData = board.save
    val name = fileName + (if (fileName contains ".") "" else ".save")
    try {
      xml.XML.save(name, boardData)
      None
    } catch {
      case _: Throwable =>
        Error write "failed to save the data!"
        Some(FileOperationError.FileNotFoundError(fileName))
    }
  }

  /**
    * Loads a [[framework.ChessBoard]] from a file and adds `.save`
    * if the path does not contain any file extension.
    *
    * @see [[framework.ChessBoard#loadExactPath loadExactPath]]
    * @param path the file
    * @return a board or [[scala.None]] when the board was saved in a different version.
    */
  def load(path: String)(implicit io: ChessIO): Either[FileOperationError.FileOperationError, ChessBoard] = {
    val fullPath = path + (if (path contains ".") "" else ".save")
    loadExactPath(fullPath)
  }

  /**
    * Loads a board from a path without alternating it.
    *
    * @see [[framework.ChessBoard#load load]]
    */
  def loadExactPath(path: String)(implicit io: ChessIO): Either[FileOperationError.FileOperationError, ChessBoard] =
    try SaveLoader.load(xml.XML.load(path))
    catch {
      case _: Throwable =>
        Left(FileOperationError.FileNotFoundError(path))
    }

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
    * Standard values for a classical chess game.
    */
  object ClassicalValues {
    /** @return the row where all pawns of this color are placed. */
    def pawnStartLine(color: AnyColor): Int =
      piecesStartLine(color) + pawnDir(color)

    /** @return the row where all pieces of this color are generated. */
    def piecesStartLine(color: AnyColor): Int =
      if (color == White) 1 else 8

    /**
      * The moving-direction of pawns of this color (i.e. the line difference they are taking in every normal move)
      *
      * @return `1` for [[framework.White]] and `-1` for [[framework.Black]]
      */
    def pawnDir(color: AnyColor): Int =
      if (color == White) 1 else -1

    /** @return the [[framework.Sqr]] where the king of this color is placed. */
    def kingStartSquare(color: AnyColor): Sqr =
      Sqr('e', piecesStartLine(color))
  }

}
