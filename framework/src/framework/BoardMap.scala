package framework

import framework.ChessBoard.columnLetter

import scala.collection.IndexedSeqLike
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.ArrayBuffer
import scala.xml._

/**
  * A chess board. Stores pieces in a 2 dimensional system (columns and rows).
  *
  * @author Felix Lehner
  * @version
  */
final case class BoardMap(ps: Array[Array[Piece]])
  extends IndexedSeq[IndexedSeq[Piece]] with IndexedSeqLike[IndexedSeq[Piece], BoardMap] {

  override val length = 8

  override def size = 8

  val pieces: Array[Array[Piece]] = if (ps.length == 8 && ps.forall(_.length == 8)) ps else Array.fill(8)(Array.fill(8)(NoPiece))

  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[scala.None]] if the sqr does not exist
    */
  def getPiece(sqr: Sqr): Option[Piece] =
    if (sqr.isValid) Some(apply(sqr._1)(sqr._2))
    else None

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
    * Updates the board.
    *
    * @param square the coordinate of the square to updated
    * @param piece  the piece the square shall be updated to
    * @return a ChessBoard with updated squares.
    */
  def updated(square: Sqr, piece: Piece): BoardMap = {
    if (square.isValid) {
      val updated = this(square._1).updated(square._2, piece)
      this.updated(square._1, updated)
    } else this
  }

  /**
    * Empties a square.
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: Sqr): BoardMap =
    if (sqr.isValid) updated(sqr, NoPiece)
    else this

  /**
    * Makes a chess move.
    * Expects the move to be legal.
    *
    * @param from       start
    * @param to         end
    * @param piece      the moving piece
    * @return the board after the move
    */
  def movePiece(from: Sqr, to: Sqr, piece: Piece): BoardMap = {
    var result = this

    //testing for en passant and castling
    piece match {
      case Pawn(_, _) if apply(to).isEmpty && from.column != to.column =>
        result = result.emptySquare(Sqr(to.column, from.row))
      case King(color, moved) if !moved && (to.column == 'c' || to.column == 'g') =>
        val row = ChessBoard.ClassicalValues.piecesStartLine(color)
        val col = if (to.column == 'c') 'd' else 'f'
        val emptyCol = if (to.column == 'c') 'a' else 'h'
        result = result.updated(Sqr(col, row), Rook(color)).emptySquare(Sqr(emptyCol, row))
      case _ =>
    }

    val resPiece = Piece(piece.identifier, piece.color, moved = true)
    result.updated(to, resPiece).emptySquare(from)
  }

  def toXml: NodeSeq = {
    for (x <- 1 to 8; col = columnLetter(x)) yield <col>{squares(col).saveData}</col> copy (label = col.toUpper toString)

    // in Column:
    /**
      * Saves this column as xml.
      */
    def saveData: IndexedSeq[scala.xml.Elem] = {
      var result: IndexedSeq[scala.xml.Elem] = IndexedSeq()
      pieces.indices.foreach(i => if (pieces(i).nonEmpty) result = result :+ pieces(i).toXml.copy(label = "l" + (i + 1)))
      result
    }
  }

  /**
    * Formats the board as a [[String]].
    *
    * @usecase Used in consoleUI to show the board in console
    * @return a formatted representation of the board
    */
  override def toString: String = {
    val separationLine: String = "  +---+---+---+---+---+---+---+---+\n"
    val lines = for (x <- 1 to 8; c = ChessBoard.columnLetter(x)) yield c + " " + apply(c)
    lines mkString("    1   2   3   4   5   6   7   8\n" + separationLine, "\n" + separationLine, "\n" + separationLine)
  }
}


object BoardMap {
  def apply(pieces: Seq[Piece]*): BoardMap = fromSeq(pieces)

  def fromSeq(buf: Seq[Seq[Piece]]): BoardMap = new BoardMap(buf.toArray.map(_.toArray))

  def newBuilder: scala.collection.mutable.Builder[Seq[Piece], BoardMap] = new ArrayBuffer mapResult fromSeq

  implicit def canBuildFrom: CanBuildFrom[BoardMap, Seq[Piece], BoardMap] =
    new CanBuildFrom[BoardMap, Seq[Piece], BoardMap] {

      def apply(): scala.collection.mutable.Builder[Seq[Piece], BoardMap] = newBuilder

      def apply(from: BoardMap): scala.collection.mutable.Builder[Seq[Piece], BoardMap] = newBuilder
    }
}
