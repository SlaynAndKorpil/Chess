package framework

import scala.language.implicitConversions

/**
  * A coordinate of a square.
  *
  * @version alpha 0.1
  * @author Felix Lehner
  */
case class Sqr(column: Int, row: Int) {
  @inline
  def _1: Int = column

  @inline
  def _2: Int = row

  def to(square: Sqr): List[Sqr] = {
    val inc = (square.column - column).signum -> (square.row - row).signum
    val incremented: Sqr = this + inc
    this :: (if (incremented == this) Nil else incremented to square)
  }

  def until(square: Sqr): List[Sqr] = {
    val inc = (square.column - column).signum -> (square.row - row).signum
    val incremented = this + inc
    this :: (if (incremented == square) Nil else incremented until square)
  }

  def +(sq: Sqr): Sqr = Sqr(column + sq.column, row + sq.row)

  def -(sq: Sqr): Sqr = Sqr(column - sq.column, row - sq.row)

  def -(sq: (Int, Int)): Sqr = Sqr(column - sq._1, row - sq._2)

  /**
    * All adjacent squares.
    *
    * @see [[framework.Sqr#validAdjacents]] for
    *      adjacent squares that are on the board.
    */
  def adjacents: IndexedSeq[Sqr] = IndexedSeq(
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1)
  ) map (this + _)

  def +(sq: (Int, Int)): Sqr = Sqr(column + sq._1, row + sq._2)

  def toTuple: (Int, Int) = (column, row)

  override def toString: String = s"${ChessBoard.columnLetter(column)}$row"
}

object Sqr {
  def apply(coordinate: String): Option[Sqr] =
    if (coordinate.length == 2 && coordinate.head.isLetter && coordinate.last.isDigit) {
      val col = coordinate.head
      val row = coordinate.last.asDigit
      Some(Sqr(col, row))
    }
    else None

  def apply(column: Char, row: Int): Sqr = Sqr(ChessBoard.columnIndex(column), row)

  def apply(sqr: (Int, Int)): Sqr = Sqr(sqr._1, sqr._2)
}
