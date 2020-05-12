package framework

import framework.ChessBoard.columnLetter

import scala.xml._

/**
  * A chess board. Stores pieces in a 2 dimensional system (columns and rows).
  *
  * @author Felix Lehner
  * @version
  */
final case class BoardMap(pieces: Array[Array[Piece]]) {
  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[scala.None]] if the sqr does not exist
    */
  def getPiece(sqr: Sqr): Option[Piece] =
    if (isValid(sqr)) Some(pieces(sqr._1 - 1)(sqr._2 - 1))
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
    * Returns the unchanged BoardMap object if coordinates are invalid.
    *
    * @param square the coordinate of the square to updated
    * @param piece  the piece the square shall be updated to
    * @return a BoardMap with updated squares.
    */
  def updated(square: Sqr, piece: Piece): BoardMap = {
    if (isValid(square)) {
      val updated = pieces(square._1 - 1).updated(square._2 - 1, piece)
      BoardMap(pieces.updated(square._1 - 1, updated))
    } else this
  }

  /**
    * Checks if a pair of coordinates describes a valid position on this BoardMap.
    *
    * @param sqr the given [[framework.Sqr]]
    * @return `true` if sqr is valid, otherwise `false`
    */
  def isValid(sqr: Sqr): Boolean = {
    val outerLen = pieces.length

    // needs to be lazy because pieces(0) might fail so we first need to test for outerLen > 0
    lazy val innerLen = pieces(0).length

    outerLen > 0 && innerLen > 0 &&
      0 < sqr.column && sqr.column <= pieces.length && 0 < sqr.row && sqr.row <= pieces(0).length
  }

  /**
    * Empties a square by placing a [[framework.NoPiece]] at this position.
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: Sqr): BoardMap = updated(sqr, NoPiece)

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
    for (x <- 0 to 7; col = columnLetter(x + 1)) yield
      <col>{
        var result: IndexedSeq[scala.xml.Elem] = IndexedSeq()
        pieces(x).indices.foreach(i =>
          if (pieces(x)(i).nonEmpty) result = result :+ pieces(x)(i).toXml.copy(label = "l" + (i + 1)))
        result
        }</col> copy (label = col.toUpper.toString)
  }

  def filter(predicate: Piece => Boolean): BoardMap =
    BoardMap(pieces map ( _.filter(predicate) ))

  /**
    * Formats the board as a [[String]].
    *
    * @usecase Used in consoleUI to show the board in console
    * @return a formatted representation of the board
    */
  override def toString: String = {
    val separationLine: String = "  +---+---+---+---+---+---+---+---+\n"
    val lines = for (x <- 0 to 7; c = ChessBoard.columnLetter(x + 1)) yield c + " " + pieces(x)
    lines mkString("    1   2   3   4   5   6   7   8\n" + separationLine, "\n" + separationLine, "\n" + separationLine)
  }
}


object BoardMap {
  def fill(piece: Piece): BoardMap = BoardMap(Array.fill(8)(Array.fill(8)(piece)))

  def empty(): BoardMap = fill(NoPiece)
}
