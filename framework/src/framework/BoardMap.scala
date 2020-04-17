package framework

/**
  * A chess board. Stores pieces in a 2 dimensional system (columns and rows).
  *
  * @author Felix Lehner
  * @version
  */
final case class BoardMap(key1: Char, value1: Column,
                          key2: Char, value2: Column,
                          key3: Char, value3: Column,
                          key4: Char, value4: Column,
                          key5: Char, value5: Column,
                          key6: Char, value6: Column,
                          key7: Char, value7: Column,
                          key8: Char, value8: Column) extends AbstractMap[Char, Column] with Map[Char, Column] with Serializable {

  override def size = 8

  override def apply(key: Char): Column =
    if (key == key1) value1
    else if (key == key2) value2
    else if (key == key3) value3
    else if (key == key4) value4
    else if (key == key5) value5
    else if (key == key6) value6
    else if (key == key7) value7
    else if (key == key8) value8
    else throw new NoSuchElementException("key not found: " + key)

  override def contains(key: Char): Boolean =
    (key == key1) || (key == key2) || (key == key3) || (key == key4) || (key == key5) || (key == key6) || (key == key7) || (key == key7) || (key == key8)

  def get(key: Char): Option[Column] =
    if (key == key1) Some(value1)
    else if (key == key2) Some(value2)
    else if (key == key3) Some(value3)
    else if (key == key4) Some(value4)
    else if (key == key5) Some(value5)
    else if (key == key6) Some(value6)
    else if (key == key7) Some(value7)
    else if (key == key8) Some(value8)
    else None

  def iterator = Iterator((key1, value1), (key2, value2), (key3, value3), (key4, value4), (key5, value5), (key6, value6), (key7, value7), (key8, value8))

  def +[V1 >: Column](kv: (Char, V1)): Map[Char, V1] = throw new UnsupportedOperationException("size of this map limited to 8")

  def updated(key: Char, value: Column): BoardMap =
    if (key == key1) new BoardMap(key1, value, key2, value2, key3, value3, key4, value4, key5, value5, key6, value6, key7, value7, key8, value8)
    else if (key == key2) new BoardMap(key1, value1, key2, value, key3, value3, key4, value4, key5, value5, key6, value6, key7, value7, key8, value8)
    else if (key == key3) new BoardMap(key1, value1, key2, value2, key3, value, key4, value4, key5, value5, key6, value6, key7, value7, key8, value8)
    else if (key == key4) new BoardMap(key1, value1, key2, value2, key3, value3, key4, value, key5, value5, key6, value6, key7, value7, key8, value8)
    else if (key == key5) new BoardMap(key1, value1, key2, value2, key3, value3, key4, value4, key5, value, key6, value6, key7, value7, key8, value8)
    else if (key == key6) new BoardMap(key1, value1, key2, value2, key3, value3, key4, value4, key5, value5, key6, value, key7, value7, key8, value8)
    else if (key == key7) new BoardMap(key1, value1, key2, value2, key3, value3, key4, value4, key5, value5, key6, value6, key7, value, key8, value8)
    else if (key == key8) new BoardMap(key1, value1, key2, value2, key3, value3, key4, value4, key5, value5, key6, value6, key7, value7, key8, value)
    else throw new NoSuchElementException("key not found: " + key)

  def -(key: Char): Map[Char, Column] = throw new UnsupportedOperationException("size of this map has to be 8")

  override def foreach[U](f: ((Char, Column)) => U): Unit = {
    f((key1, value1))
    f((key2, value2))
    f((key3, value3))
    f((key4, value4))
    f((key5, value5))
    f((key6, value6))
    f((key7, value7))
    f((key8, value8))
  }

  /**
    * @param sqr coordinates of the wanted piece
    * @return the chess square at a specific position on the board, [[scala.None]] if the sqr does not exist
    */
  def getPiece(sqr: Square): Option[Piece] =
    if (sqr.isValid) Some(apply(sqr._1).pieceAt(sqr._2))
    else None

  /**
    * Gives the piece at any position.
    * When the coordinate is outside the board, [[framework.NoPiece NoPiece]] is returned.
    *
    * @param sqr coordinates on the board
    * @return the piece at some specified position
    */
  @inline
  def apply(sqr: Square): Piece = getPiece(sqr) getOrElse NoPiece

  /**
    * Updates the board.
    *
    * @param square the coordinate of the square to updated
    * @param piece  the piece the square shall be updated to
    * @return a ChessBoard with updated squares.
    */
  def updated(square: Square, piece: Piece): BoardMap = {
    val updated = this(square._1).updated(square._2, piece)
    this.updated(square._1, updated)
  }

  /**
    * Empties a square.
    *
    * @param sqr the square to be emptied
    */
  def emptySquare(sqr: Square): BoardMap =
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
  def movePiece(from: Square, to: Square, piece: Piece): BoardMap = {
    var result = this

    //testing for en passant and castling
    piece match {
      case Pawn(_, _) if apply(to).isEmpty && from.column != to.column =>
        result = result.emptySquare(Square(to.column, from.row))
      case King(color, moved) if !moved && (to.column == 'c' || to.column == 'g') =>
        val row = ChessBoard.ClassicalValues.piecesStartLine(color)
        val col = if (to.column == 'c') 'd' else 'f'
        val emptyCol = if (to.column == 'c') 'a' else 'h'
        result = result.updated(Square(col, row), Rook(color)).emptySquare(Square(emptyCol, row))
      case _ =>
    }

    val resPiece = Piece(piece.identifier, piece.color, moved = true)
    result.updated(to, resPiece).emptySquare(from)
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
