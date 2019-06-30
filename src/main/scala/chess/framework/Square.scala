package chess.framework

import chess.framework.ChessBoard.{columnIndex, columnLetter}

import scala.language.implicitConversions

sealed abstract class AbstractSqrCoordinate[ColumnType] {
  val column: ColumnType
  val row: Int

  type Return >: this.type <: AbstractSqrCoordinate[ColumnType]

  @inline
  def _1: ColumnType = column

  @inline
  def _2: Int = row

  def colIndx: Int

  def to(square: AbstractSqrCoordinate[ColumnType]): List[Return] = {
    val inc = (square.colIndx - colIndx).signum -> (square.row - row).signum
    val incremented: Return = this + inc
    val head: Return = this
    val tail: List[Return] =
      if (incremented == this) Nil
      else incremented.to(square).asInstanceOf[List[Return]]
    head :: tail
  }

  def until(square: AbstractSqrCoordinate[ColumnType]): List[Return] = {
    val inc = (square.colIndx - colIndx).signum -> (square.row - row).signum
    val incremented = this + inc
    val head: Return = this
    val tail: List[Return] =
      if (incremented == square) Nil
      else incremented.until(square).asInstanceOf[List[Return]]
    head :: tail
  }

  def +(sq: AbstractSqrCoordinate[_]): Return

  def -(sq: AbstractSqrCoordinate[_]): Return

  def +(sq: (Int, Int)): Return

  def -(sq: (Int, Int)): Return

  def unary_+ : Square = NumericSquare(colIndx, row)

  def unary_- : Square = NumericSquare(-colIndx, -row)

  def adjacents: IndexedSeq[Return] = IndexedSeq(
    (-1, -1), (-1, 0), (-1, 1),
    (0, -1), (0, 1),
    (1, -1), (1, 0), (1, 1)
  ) map (this + _)

  def validAdjacents: IndexedSeq[Return] = adjacents filter (_.isValid)

  def toTuple: (ColumnType, Int) = (column, row)

  /**
    * Checks if this square is inside of a 8x8 board.
    */
  def isValid: Boolean = colIndx >= 1 && colIndx <= 8 && row >= 1 && row <= 8

  override def toString: String = s"$column$row"
}


final case class Square(column: Char, row: Int) extends AbstractSqrCoordinate[Char] {
  val colIndx: Int = columnIndex(column)

  override type Return = Square

  override def +(sq: AbstractSqrCoordinate[_]): Return = sq match {
    case Square(col, line) =>
      Square(columnLetter(columnIndex(column)+columnIndex(col)), row + line)
    case NumericSquare(col, line) => Square(columnLetter(columnIndex(column) + col), row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): Square = this.+(-sq)

  override def +(sq: (Int, Int)): Square = Square(columnLetter(columnIndex(column) + sq._1), row + sq._2)

  override def -(sq: (Int, Int)): Square = this.+(-sq._1, -sq._2)
}

object Square {
  def apply(coordinate: String): Option[Square] =
    if (coordinate.length == 2 && coordinate.head.isLetter && coordinate.last.isDigit) {
      val col = coordinate.head
      val row = coordinate.last.asDigit
      Some(Square(col, row))
    }
    else None
}


final case class NumericSquare(column: Int, row: Int) extends AbstractSqrCoordinate[Int] {
  def colIndx: Int = column

  override type Return = NumericSquare

  override def +(sq: AbstractSqrCoordinate[_]): NumericSquare = sq match {
    case Square(col, line) => NumericSquare(column + columnIndex(col), row + line)
    case NumericSquare(col, line) => NumericSquare(column + col, row + line)
  }

  override def -(sq: AbstractSqrCoordinate[_]): NumericSquare = this.+(-sq)

  override def +(sq: (Int, Int)): NumericSquare = NumericSquare(column + sq._1, row + sq._2)

  override def -(sq: (Int, Int)): NumericSquare = this.+(-sq._1, -sq._2)
}

object NumericSquare {
  def apply(tup: (Int, Int)) = new NumericSquare(tup._1, tup._2)
}


object AbstractSqrCoordinate {
  /**
    * implicit conversion from [[chess.framework.Square]] to [[chess.framework.NumericSquare]]
    */
  implicit def sqr2indxSqr(sqr: Square): NumericSquare = NumericSquare(sqr.colIndx, sqr.row)

  /**
    * implicit conversion from [[chess.framework.NumericSquare]] to [[chess.framework.Square]]
    */
  implicit def indxSqr2sqr(iSqr: NumericSquare): Square = Square(columnLetter(iSqr.column), iSqr.row)
}
